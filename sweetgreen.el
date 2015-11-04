;; -*- lexical-binding: t -*-

(require 'request)
(require 'rx)
(require 'dash)
(require 'helm)

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

(defvar order-id nil)
(defvar items '())
(defvar orders '())
(defvar order-id nil)
(defvar basket-id nil)

(defun => (alist &rest keys)
  (-reduce-from (lambda (acc item) (assoc-default item acc) ) alist keys))

;; (url-hexify-string "455 broadway")
;; (setq json-object-type "hash-table")
(helm-mini)

(sweetgreen/make-order 10003 26 )

(defun sweetgreen/helm-restaurants (zip_code)
  (interactive "sZip Code: ")
  ;; (print zip_code)
  (setq sweetgreen--restaurants-alist (sweetgreen//get-restaurants zip_code))
  (helm
   :sources `((name . "Sweetgreen Restaurants")
              (candidates . ,(mapcar 'cdr sweetgreen--restaurants-alist))
              (action . (lambda (f)
                          (setq sweetgreen--curr-restaurant
                                (car (rassoc f sweetgreen--restaurants-alist)))
                          (message "You selected %s" (=> sweetgreen--restaurants-alist sweetgreen--curr-restaurant))
                          )))
   :buffer "✷Sweetgreen ❤ Restaurants✷"))

(defun sweetgreen/helm-menu (restaurant_id)
  ;; (print zip_code)
  (setq sweetgreen--menu-alist (sweetgreen//get-menu restaurant_id))
  (helm
   :sources (make-sources restaurant_id)
   :buffer "✷Sweetgreen ❤ Menu List✷"))

(sweetgreen/helm-menu "26")

(defun make-sources (restaurant_id)
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
              :action (lambda (candidate)
                        (print candidate)))))
        sweetgreen--menu-alist))


(defun sweetgreen//get-restaurants (zip_code)
  (when (and sweetgreen--csrf-token
             sweetgreen--cookie-string)
    (let* ((restaurants-response (request
                                  "https://order.sweetgreen.com/api/restaurants"
                                  :type "GET"
                                  :sync t
                                  :params `(("zip_code" . ,zip_code))
                                  :headers `(("Cookie" . ,sweetgreen--cookie-string)
                                             ("X-CSRF-Token" . ,sweetgreen--csrf-token))
                                  :parser 'json-read))
           (restaurants-data     (request-response-data restaurants-response))
           (restaurants          (=> restaurants-data 'restaurants)))
      (--map `(,(=> it 'id) .
               ,(format "%+25s     ---->     %.2f miles away"
                       (=> it 'name) (=> it 'distance)))
             restaurants))))

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
           (menu (--group-by (=> it 'category_name) le-menu)))
      menu)))

;; Get CSRF Token
(request
 "https://order.sweetgreen.com"
 :parser (lambda ()
           (let ((data (buffer-string)))
             (string-match sweetgreen--csrf-token-regexp data)
             (let ((token (match-string 1 data)))
               (setq sweetgreen--csrf-token token))))
 :complete (lambda (&rest _) (message "CSRF Token: %s" sweetgreen--csrf-token)))

;; Get Session Cookie
(request
 "https://order.sweetgreen.com/api/customers/login"
 :type "POST"
 :data `(("customer[email]" . ,username)
         ("customer[password]" . ,passwd))
 :headers '(("Content-Type" . "application/x-www-form-urlencoded; charset=UTF-8"))
 :parser 'buffer-string
 :complete (function*
            (lambda (&key data response &allow-other-keys)
              (let ((cookie-header (request-response-header response "set-cookie")))
                (string-match sweetgreen--cookie-regexp cookie-header)

                (setq sweetgreen--cookie-string (concat "_session_id="
                                         (match-string 1 cookie-header)))
                (message "Cookie collected: %s" sweetgreen--cookie-string)))))

;; Add Item to cart
(request
 "https://order.sweetgreen.com/api/line_items"
 :type "POST"

 :data (json-encode `(("line_item" . (("quantity" . 1)
                                      ("product_id" . 1947)
                                      ("restaurant_id" . 26)
                                      ("calories" . 410)))))
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
                (push `(,item_id . ,item) items)))))

;; Get basket Id
(request
 "https://order.sweetgreen.com/api/orders"
 :type "GET"
 :params `(("id" . ,order-id))
 :headers `(("Content-Type" . "application/json")
            ("Cookie" . ,sweetgreen--cookie-string)
            ("X-CSRF-Token" . ,sweetgreen--csrf-token))

 :parser 'json-read
 :complete (function*
            (lambda (&key data response &allow-other-keys)
              (let* ((order     (aref (=> data 'orders) 0))
                     (basket_id (=> order 'basket_id)))
                (setq basket-id basket_id)
                (push `(,basket_id . ,order) orders))))
 )

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
