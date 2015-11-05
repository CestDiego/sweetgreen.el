;; -*- lexical-binding: t -*-
;;; sweetgreen.el --- Order Salads from https://sweetgreen.com from inside Emacs

;; Copyright (C) 2015 Diego Berrocal

;; Author: Diego Berrocal <cestdiego@gmail.com>
;; Created: 3 November 2015

;; Keywords: salad, food, sweetgreen, request
;; Homepage: http://www.github.com/CestDiego/sweetgreen.el/
;; Version: 0.0.1
;; Package-Requires: ((dash "2.12.1") (helm "1.5.6) (request "0.2.0"))

;; This file is not part of GNU Emacs.

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Commentary:

;; Give your Emacs the power to order salads and leave a healthier lifestyle,
;; don't forget to also exercise!

;;; Code:

(require 'request)
(require 'dash)
(require 'helm)

(defgroup sweetgreen nil
  "Order a variety of products from Sweetgreen without leaving your editor."
  :group 'sweetgreen)

(defcustom sweetgreen--username nil
  "Sweetgreen Accounr Username"
  :type 'string
  :group 'sweetgreen)
(defvar sweetgreen--password nil
  "Sweetgreen Account Password")

(defvar sweetgreen--csrf-token-regexp "<meta content=\"\\([^\"]+\\).*?csrf-token.*?>"
  "Regular Expression used to grab the CSRF Token from the index page.")

(defvar sweetgreen--cookie-regexp "_session_id=\\([^;]+\\)"
  "Regular expression to get the Session ID from the response's headers.")

(defvar sweetgreen--csrf-token ""
  "CSRF Token for http://orders.sweetgreen.com")
(defvar sweetgreen--cookie-string ""
  "Cookies for http://orders.sweetgreen.com")

(defvar sweetgreen--restaurants-alist '()
  "Nearby Restaurants alist")

(defvar sweetgreen--menu-alist '()
  "Menu for Current restaurant")
(defvar sweetgreen--curr-restaurant nil
  "Current Restaurant")

(defvar sweeetgreens--default-restaurant nil
  "Default restaurant id")

(defvar order-id nil)
(defvar items '())
(defvar orders '())
(defvar order-id nil)
(defvar basket-id nil)

(defun => (alist &rest keys)
  (-reduce-from (lambda (acc item) (assoc-default item acc) ) alist keys))

;; (url-hexify-string "455 broadway")
;; (setq json-object-type "hash-table")

(defun sweetgreen//auth (&optional username password)
  (interactive)
  (if (and sweetgreen--username username)
      (setq sweetgreen--username (read-from-minibuffer "Username: "))
    (setq sweetgreen--username username))
  (if (and sweetgreen--password password)
      (setq sweetgreen--password (read-passwd "Super Secret Password: "))
    (setq sweetgreen--password password))

  (sweetgreen//fetch-csrf-token)
  (sweetgreen//fetch-auth-cookie username password))

(defun sweetgreen//fetch-csrf-token ()
  (let* ((response (request
                   "https://order.sweetgreen.com"
                   :type "GET"
                   :sync t
                   :parser 'buffer-string
                   :error
                   (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                                (error "Got error: %S" error-thrown)))
                   ))
        (data  (request-response-data response))
        (csrf-token (progn
                      (string-match sweetgreen--csrf-token-regexp data)
                      (match-string 1 data))))
    (setq sweetgreen--csrf-token csrf-token)))

(defun sweetgreen//fetch-auth-cookie (username password)
  (let* ((response (request
                    "https://order.sweetgreen.com/api/customers/login"
                    :type "POST"
                    :sync t
                    :data `(("customer[email]" . ,username)
                            ("customer[password]" . ,password))
                    :headers '(("Content-Type" . "application/x-www-form-urlencoded; charset=UTF-8"))
                    :parser 'buffer-string
                    :error
                    (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                                 (error "Got error: %S" error-thrown)))
                    ))
         (header (request-response-header response "set-cookie"))
         (cookie-string (progn
                          (string-match sweetgreen--cookie-regexp header)
                          (concat "_session_id=" (match-string 1 header)))))
    (setq sweetgreen--cookie-string cookie-string)))

(defun sweetgreen ()
  (interactive)
  ;; Get CSRF Token and Cookie Headers
  (call-interactively 'sweetgreen//auth [sweetgreen--username sweetgreen--password])
  ;; Get Current Restaurant and Item to Buy
  (if sweeetgreens--default-restaurant
      (sweetgreen/helm-menu sweeetgreens--default-restaurant)
    (let* ((curr-restaurant    (call-interactively 'sweetgreen/helm-restaurants))
           (curr-restaurant-id (number-to-string (=> curr-restaurant 'id)))
           (curr-product       (sweetgreen/helm-menu curr-restaurant-id)))

      (print curr-restaurant)
      (sweetgreen/add-to-cart curr-product)
      )
    )
  )

(defun sweetgreen/helm-restaurants (zip_code)
  (interactive "sZip Code: ")
  (let* ((restaurant-alist (sweetgreen//get-restaurants zip_code))
         )
    (setq sweetgreen--restaurants-alist restaurant-alist)
    (helm
     :sources
     (helm-build-sync-source "Sweetgreen Restaurants"

       :candidates restaurant-alist
       :candidate-transformer (lambda (candidates)
                                (--map
                                 `(,(format
                                     "%+25s     ---->     %.2f miles away"
                                     (=> (cdr it) 'name) (=> (cdr it) 'distance))
                                   . ,it)
                                 candidates)
                                )
       :persistent-action (lambda (selected_restaurant)
                            (browse-url
                             (concat "https://order.sweetgreen.com/"
                                     (=> selected_restaurant 'restaurant_slug))))
       :action (lambda (candidate)
                 (setq sweetgreen--curr-restaurant candidate)
                 (print candidate)))
     :buffer "✷Sweetgreen ❤ Restaurants✷")))

(defun sweetgreen/helm-menu (restaurant_id)
  (unless restaurant_id
    (error "No Restaurant ID specified"))
  (setq sweetgreen--menu-alist (sweetgreen//get-menu restaurant_id))
  (helm
   :sources (sweetgreens//make-helm-menu-sources restaurant_id)
   :buffer "✷Sweetgreen ❤ Menu List✷"))

(defun sweetgreens//make-helm-menu-sources (restaurant_id)
  (-map (lambda (menu)
          (let* ((name (upcase-initials (car menu)))
                 (menu-list (cdr menu))
                 (menu-alist (--map `(,(format "%+35s     ---->       %.2f"
                                               (upcase-initials (=> it 'name))
                                               (/ (=> it 'cost) 100))
                                      . ,it)
                                    menu-list)))
            (helm-build-sync-source name
              :candidates menu-alist
              :persistent-action (lambda (candidate)
                                   (browse-url
                                    (concat "https://order.sweetgreen.com/nolita/"
                                            (=> candidate 'product_slug))))
              :action (lambda (candidate)
                        (print candidate)
                        candidate))))
        sweetgreen--menu-alist))

(defun sweetgreen//get-restaurants (zip_code)
  (when (and sweetgreen--csrf-token
             sweetgreen--cookie-string)
    (let* ((response    (request
                         "https://order.sweetgreen.com/api/restaurants"
                         :type "GET"
                         :sync t
                         :params `(("zip_code" . ,zip_code))
                         :headers `(("Cookie" . ,sweetgreen--cookie-string)
                                    ("X-CSRF-Token" . ,sweetgreen--csrf-token))
                         :parser 'json-read))
           (data        (request-response-data response))
           (restaurants (--map `(,(number-to-string (=> it 'id)) . ,it)
                               (=> data 'restaurants))))
      restaurants)))

(defun sweetgreen//get-menu (restaurant_id)
  (when (and sweetgreen--csrf-token
             sweetgreen--cookie-string)
    (let* ((menu-response (request
                           (concat "https://order.sweetgreen.com/api/menus/"
                                   restaurant_id)
                           :type "GET"
                           :sync t
                           :headers `(("Cookie" . ,sweetgreen--cookie-string)
                                      ("X-CSRF-Token" . ,sweetgreen--csrf-token))
                           :parser 'json-read))
           (menu-data     (request-response-data menu-response))
           (products      (append (=> menu-data 'products) nil))
           (menu (--group-by (=> it 'category_name) products)))
      menu)))

;; Add Item to cart
(defun sweetgreen/add-to-cart (product)
  (request
   "https://order.sweetgreen.com/api/line_items"
   :type "POST"
   :data (json-encode `(("line_item" . (("quantity" . 1)
                                        ("product_id" . ,(=> product 'id))
                                        ("restaurant_id" . ,(=> product 'restaurant_id))
                                        ("calories" . ,(=> product 'calories))
                                        )
                         )))
   :headers `(("Content-Type" . "application/json")
              ("Cookie" . ,sweetgreen--cookie-string)
              ("X-CSRF-Token" . ,sweetgreen--csrf-token))

   :parser 'json-read
   :complete (function*
              (lambda (&key data response &allow-other-keys)
                (let* ((item     (=> data 'line_item))
                       (item_id  (=> item 'id))
                       (order_id (=> item 'ignored_order_id)))
                  (setq order-id (number-to-string order_id))
                  (push `(,item_id . ,item) items))))))


;; Get basket Id
(defun sweetgreen//fetch-basket-id ()
  (let* ((response  (request
                     "https://order.sweetgreen.com/api/orders"
                     :type    "GET"
                     :sync    t
                     :params  `(("id" . ,order-id))
                     :headers `(("Content-Type" . "application/json")
                                ("Cookie" . ,sweetgreen--cookie-string)
                                ("X-CSRF-Token" . ,sweetgreen--csrf-token))

                     :parser 'json-read
                     :error
                     (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                                  (error "Got error: %S" error-thrown)))))
         (data       (request-response-data response))
         (order      (aref (=> data 'orders) 0))
         (basket_id (=> order 'basket_id)))
    (push `(,basket_id . ,order) orders)))

(defun cancel-item (id)
  (request
   (concat "https://order.sweetgreen.com/api/line_items/" (number-to-string id))
   :type "DELETE"
   :headers `(("Cookie" . ,(concat "_session_id=" sweetgreen--cookie-string))
              ("X-CSRF-Token" . ,sweetgreen--csrf-token))

   :parser 'json-read
   :status-code '((204 . (lambda (&rest _) (message "Deleted item successfully")))
                  (500 . (lambda (&rest _) (message "Item doesn't seem to exist"))))))


(defun checkout (order)
  (setq order (=> orders basket-id))
  (request
   (concat "https://order.sweetgreen.com/api/line_items/" (=> order 'id))
   :type "PUT"
   :headers `(("Cookie" . ,(concat "_session_id=" sweetgreen--cookie-string))
              ("X-CSRF-Token" . ,sweetgreen--csrf-token))
   :data `(json-encode
           `(("order" . (("available_wanted_times_tuples" . (=> order 'available_wanted_times_tuples))
                         ("basket_id" . (=> order 'basket_id))
                         ("created_at" . (=> order 'created_at))
                         ("coupon_code" . (=> order 'coupon_code))
                         ("coupon_discount" . (=> order 'coupon_discount))
                         ("placed_time" . (=> order 'placed_time))
                         ("formatted_wanted_time" . (=> order 'formatted_wanted_time))
                         ("restaurant_id" . (=> order 'restaurant_id))
                         ("sales_tax" . (=> order 'sales_tax))
                         ("subtotal" . (=> order 'subtotal))
                         ("total" . (=> order 'total))
                         ("shows_feedback_form" . (=> order 'shows_feedback_form))
                         ("uploaded_at")
                         ("contact_number" . "***REMOVED***")
                         ("state" . "complete")
                         ("wanted_time" . (=> order 'earliest_wanted_time))
                         ("billing_account" .
                          (("card_type" . "cash")
                           ("card_number")
                           ("zip")
                           ("last_four")
                           ("cvv")
                           ("expiry_month")
                           ("expiry_year")
                           ("description" . "sweetgreen Rewards (Pay with App)")
                           ("save_on_file" . :json-false)
                           ))))))
   :parser 'json-read
   :complete (function*
              (lambda (&key data response &allow-other-keys)
                (let* ((order     (aref (=> data 'orders) 0))
                       (basket_id (=> order 'basket_id)))
                  (print data)
                  )))
  ))
