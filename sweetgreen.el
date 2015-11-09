;;; sweetgreen.el --- Order Salads from https://sweetgreen.com from inside Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015 Diego Berrocal

;; Created: 3 November 2015
;; Author: Diego Berrocal <cestdiego@gmail.com>
;; Homepage: https://www.github.com/CestDiego/sweetgreen.el
;; Version: 1.0.0
;; Package-Requires: ((dash "2.12.1") (helm "1.5.6") (request "0.2.0"bb) (cl-lib "0.5"))
;; Keywords: salad, food, sweetgreen, request

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

;;; Usage:

;;     (require 'sweetgreen) ;; Not necessary if using ELPA package
;;     M-x sweetgreen

;;; Code:
(require 'request)
(require 'dash)
(require 'helm)
(require 'cl-lib)

(defgroup sweetgreen nil
  "Order a variety of products from Sweetgreen without leaving your editor."
  :group 'sweetgreen)

(defcustom sweetgreen--username nil
  "Sweetgreen Accounr Username"
  :type 'string
  :group 'sweetgreen)

(defcustom sweetgreen--password nil
  "Sweetgreen Account Password"
  :type 'string
  :group 'sweetgreen)

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
(defvar sweetgreen--products-alist '()
  "Menu for Current restaurant")
(defvar sweetgreen--curr-restaurant nil
  "Current Restaurant")

(defvar sweetgreen--available-times nil)

(defvar sweetgreen--items-alist '())
(defvar sweetgreen--curr-basket '())
(defvar sweetgreen--curr-user      nil)
(defvar sweetgreen--curr-order-id  nil)
(defvar sweetgreen--curr-basket-id nil)
(defvar basket-id nil)

(defun => (alist &rest keys)
  (-reduce-from (lambda (acc item) (assoc-default item acc) ) alist keys))

(defun sweetgreen//auth (&optional username password)
  (interactive)
  (unless sweetgreen--username (setq sweetgreen--username
                                     (read-from-minibuffer "Username: ")))
  (unless sweetgreen--password (setq sweetgreen--password
                                     (read-passwd "Super Secret Password: ")))
  (sweetgreen//fetch-csrf-token)
  (sweetgreen//fetch-auth-cookie username password))

(defun sweetgreen//fetch-csrf-token ()
  (let* ((response (request
                   "https://order.sweetgreen.com"
                   :type "GET"
                   :sync t
                   :parser 'buffer-string
                   :error
                   (cl-function (lambda (&key data error-thrown &allow-other-keys&rest _)
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
                    :headers '(("Accept"       . "application/json")
                               ("Content-Type" . "application/x-www-form-urlencoded"))
                    :parser 'json-read
                    :error
                    (cl-function (lambda (&key data error-thrown &allow-other-keys&rest _)
                                   (error "Got error: %S" error-thrown)))
                    ))
         (header (request-response-header response "set-cookie"))
         (data (request-response-data response))
         (cookie-string (progn
                          (string-match sweetgreen--cookie-regexp header)
                          (concat "_session_id=" (match-string 1 header)))))
    (setq sweetgreen--curr-user     (=> data 'customer) )
    (setq sweetgreen--cookie-string cookie-string))
  )

(defun sweetgreen//logout (curr-user)
  (unless curr-user
    (error "You try to log out but you are not logged in m8"))
  (let* ((response (request
                    (format "https://order.sweetgreen.com/api/customers/%.0f"
                            (=> sweetgreen--curr-user 'id))
                    :type "DELETE"
                    :sync t
                    :headers '(("Accept"       . "application/json")
                               ("Content-Type" . "application/x-www-form-urlencoded")
                               ("X-CSRF-Token" . ,sweetgreen--csrf-token))
                    :parser 'buffer-string))
         (header (request-response-header response "set-cookie"))
         (cookie-string (progn
                          (string-match sweetgreen--cookie-regexp header)
                          (concat "_session_id=" (match-string 1 header))))
         )
    (setq sweetgreen--curr-user nil)
    (setq sweetgreen--cookie-string cookie-string)))

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
                 (setq sweetgreen--curr-restaurant candidate)))
     :buffer "*Sweetgreen ❤ Restaurants*")))

(defun sweetgreen/helm-wanted-time (order_id)
  (unless restaurant_id
    (error "No Restaurant ID specified"))
  (setq sweetgreen--menu-alist (sweetgreen//get-menu restaurant_id))
  (helm
   :sources (sweetgreen//make-helm-menu-sources restaurant_id)
   :buffer "*Sweetgreen ❤ Wanted Time List*"))

(defun sweetgreen/helm-menu (restaurant_id)
  (unless restaurant_id
    (error "No Restaurant ID specified"))
  (setq sweetgreen--menu-alist (sweetgreen//get-menu restaurant_id))
  (helm
   :sources (sweetgreen//make-helm-menu-sources restaurant_id)
   :buffer "*Sweetgreen ❤ Menu List*")
  )

(defun sweetgreen//make-helm-menu-sources (restaurant_id)
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
           (restaurants (--map `(,(=> it 'id) . ,it)
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
      (setq sweetgreen--products-alist (--map `(,(=> it 'id) . ,it) products))
      menu)))

(defun sweetgreen//add-to-cart (product)
  (let* ((response      (request
                         "https://order.sweetgreen.com/api/line_items"
                         :type "POST"
                         :sync t
                         :data (json-encode
                                `(("line_item" . (("quantity" . 1)
                                                  ("product_id" . ,(=> product 'id))
                                                  ("restaurant_id" . ,(=> product 'restaurant_id))
                                                  ("calories" . ,(=> product 'calories))))))
                         :headers `(("Content-Type" . "application/json")
                                    ("Cookie" . ,sweetgreen--cookie-string)
                                    ("X-CSRF-Token" . ,sweetgreen--csrf-token))
                         :parser 'json-read))
         (data          (request-response-data response))
         (item          (=> data 'line_item))
         (item_id       (=> item 'id))
         (order_id      (=> item 'ignored_order_id))
         (curr_basket (sweetgreen//fetch-basket (number-to-string order_id))))
    (push `(,item_id . ,item) sweetgreen--items-alist)
    (setq sweetgreen--curr-order-id (number-to-string order_id))
    curr_basket))

(defun sweetgreen//fetch-basket (order-id)
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
                     (cl-function (lambda (&key data error-thrown &allow-other-keys&rest _)
                                    (error "Got error: %S" error-thrown)))))
         (data       (request-response-data response))
         (order      (aref (=> data 'orders) 0)))
    (setq sweetgreen--curr-basket-id (=> order 'basket_id))
    (setq sweetgreen--available-times (=> order 'available_wanted_times_tuples))
    (setq sweetgreen--curr-basket order)
    ))

(defun sweetgreen/confirm-product (product)
  (let* ((name     (upcase-initials (=> product 'name)))
        (restaurant (=> sweetgreen--restaurants-alist (=> product 'restaurant_id)))
        (location (=> restaurant 'name))
        (address (concat (=> restaurant 'address) ", " (=> restaurant 'state)))
        (instructions (=> restaurant 'pickup_instructions))
        (random-pun (nth (random 4) '("Orange you glad you use Emacs?"
                                      "Do you like to party?? Lettuce turnip the beet!"
                                      "Don't forget to lettuce know if you came from RC"
                                      "Romaine calm! You haven't order your salad yet.")) )
        (cost     (/ (=> product 'cost) 100))
        (calories (=> product 'calories)))
    (y-or-n-p
     (format
      "%s
You are buying the %s
At the %s location @ %s
%s
Price before Taxes is $%.2f
It contains %.0f calories
Confirm your order? "
      random-pun
      name
      location
      address
      instructions
      cost
      calories))))

(defun sweetgreen//order-product (product)
  (let* ((basket (sweetgreen//add-to-cart product))
         (wanted_time (sweetgreen//select-time basket)))
    (if (y-or-n-p "really continue?")
        (checkout basket wanted_time)
      (cancel-orders basket))))

(defun sweetgreen//select-time (order)
  (unless order
    (error "You have given no order to select time"))
  (let ((available-times (--map `(,(=> it 'formatted) . ,(=> it 'original))
                                (=> order 'available_wanted_times_tuples))))
    (helm
     :sources (helm-build-sync-source "Available pickup times"
                :candidates available-times
                :action 'identity)
     :buffer "*Sweetgreen ❤ Available Pickup Times  *"
     )
    )
  )

(defun cancel-item (id)
  (let ((request-url (concat "https://order.sweetgreen.com/api/line_items/"
                             (number-to-string id))))
    (request
     request-url
     :type "DELETE"
     :headers `(("Cookie" . ,(concat "_session_id=" sweetgreen--cookie-string))
                ("X-CSRF-Token" . ,sweetgreen--csrf-token))

     :status-code '((204 . (lambda (&rest _) (message "Deleted item successfully")))
                    (500 . (lambda (&rest _) (message "Item doesn't seem to exist")))))))

(defun cancel-orders (order)
  (let ((item_ids (=> order 'line_item_ids)))
    (--map (cancel-item it) item_ids)
    (setq sweetgreen--curr-basket nil)
    (setq sweetgreen--curr-basket-id nil)))

(defun checkout (basket wanted_time)
  (setq data `(("order" .
                (
                 ("available_wanted_times_tuples" . ,(=> basket 'available_wanted_times_tuples))
                 ("basket_id"                     . ,(=> basket 'basket_id))
                 ("created_at"                    . ,(=> basket 'created_at))
                 ("coupon_code"                   . ,(=> basket 'coupon_code))
                 ("coupon_discount"               . ,(=> basket 'coupon_discount))
                 ("placed_time"                   . ,(=> basket 'placed_time))
                 ("formatted_wanted_time"         . ,(=> basket 'formatted_wanted_time))
                 ("restaurant_id"                 . ,(=> basket 'restaurant_id))
                 ("sales_tax"                     . ,(=> basket 'sales_tax))
                 ("subtotal"                      . ,(=> basket 'subtotal))
                 ("total"                         . ,(=> basket 'total))
                 ("shows_feedback_form"           . ,(=> basket 'shows_feedback_form))
                 ("wanted_time"                   . ,wanted_time)
                 ("uploaded_at")
                 ("contact_number" . "6467501189")
                 ("state" . "complete")
                 ("billing_account" .
                  (
                   ("card_type" . "cash")
                   ("card_number")
                   ("zip")
                   ("last_four")
                   ("cvv")
                   ("expiry_month")
                   ("expiry_year")
                   ("description" . "sweetgreen Rewards (Pay with App)")
                   ("save_on_file" . :json-false)))))))
  (request
   (concat "https://order.sweetgreen.com/api/orders/" (number-to-string (=> basket 'id)))
   :type "PUT"
   :headers `(("Cookie" . ,(concat "_session_id=" sweetgreen--cookie-string))
              ("Content-Type" . "application/json")
              ("X-CSRF-Token" . ,sweetgreen--csrf-token))
   :data (json-encode data)
   :parser 'json-read
   :complete (function*
              (lambda (&key data response &allow-other-keys)
                (let* ((basket     (aref (=> data 'orders) 0))
                       (basket_id (=> basket 'basket_id)))
                  (print data)
                  (message "Yeah salad is ordered")
                  )))))

;;;###autoload
(defun sweetgreen (args)
  (interactive "P")
  (when args
    (setq sweetgreen--curr-restaurant nil))
  (when sweetgreen--curr-user
    (sweetgreen//logout sweetgreen--curr-user))
  ;; Get CSRF Token and Cookie Headers
  (call-interactively 'sweetgreen//auth)
  ;; Get Current Restaurant and Item to Buy
  (if sweetgreen--curr-restaurant
      (let* ((curr-product       (sweetgreen/helm-menu
                                  (number-to-string
                                   (=> sweetgreen--curr-restaurant 'id))))
             (confirmed-product  (sweetgreen/confirm-product curr-product)))
        (when confirmed-product  (sweetgreen//order-product curr-product)))
    (let* ((curr-restaurant      (call-interactively 'sweetgreen/helm-restaurants))
           (curr-restaurant-id   (number-to-string (=> curr-restaurant 'id)))
           (curr-product         (sweetgreen/helm-menu curr-restaurant-id))
           (confirmed-product    (sweetgreen/confirm-product curr-product)))
      (when confirmed-product    (sweetgreen//order-product curr-product)))))

(provide 'sweetgreen)

;; Local Variables:
;; coding: utf-8
;; End:

;;; sweetgreen.el ends here
