;; -*- lexical-binding: t -*-
(require 'request)
(require 'rx)

(defvar csrf-token-regexp
  "<meta content=\"\\([^\"]+\\).*?csrf-token.*?>")

(setq cookie-regexp
  "session_id=\\([^;]+\\)")

(defvar csrf-token "")
(defvar order-id nil)
(defvar muh-cookie "")
(defvar items '())
(defvar orders '())
(defvar order-id nil)
(defvar basket-id nil)

(defun => (alist &rest keys)
  (-reduce-from (lambda (acc item) (assoc-default item acc) ) alist keys))

;; Get CSRF Token
(request
 "https://order.sweetgreen.com"
 :parser (lambda ()
           (let ((data (buffer-string)))
             (string-match csrf-token-regexp data)
             (let ((token (match-string 1 data)))
               (setq csrf-token token))))
 :complete (lambda (&rest _) (message "CSRF Token: %s" csrf-token)))

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
                (string-match cookie-regexp cookie-header)
                (setq muh-cookie (match-string 1 cookie-header))
                (message "Cookie collected: %s" muh-cookie)))))

;; Add Item to cart
(request
 "https://order.sweetgreen.com/api/line_items"
 :type "POST"

 :data (json-encode `(("line_item" . (("product_id" . 1947) ("quantity" . 1)
                                      ("restaurant_id" . 26)
                                      ("calories" . 410)))))
 :headers `(("Content-Type" . "application/json")
            ("Cookie" . ,(concat "_session_id=" muh-cookie))
            ("X-CSRF-Token" . ,csrf-token))

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
            ("Cookie" . ,(concat "request_method=POST; "
                                 "_session_id=" muh-cookie))
            ("X-CSRF-Token" . ,csrf-token))

 :parser 'json-read
 :complete (function*
            (lambda (&key data response &allow-other-keys)
              (let* ((order     (aref (=> data 'orders) 0))
                     (basket_id (=> order 'basket_id)))
                (setq basket-id basket_id)
                (push `(,basket_id . ,order) orders))))
 )

(setq order-id nil)
(setq basket-id nil)

(let ((created_at (=> orders basket-id 'created_at))
      (wanted_time (=> orders basket-id 'earliest_wanted_time))
      ))

(--map (cancel-item it) a)

(defun cancel-item (id)
  (request
   (concat "https://order.sweetgreen.com/api/line_items/" (number-to-string id))
   :type "DELETE"
   :headers `(("Cookie" . ,(concat "_session_id=" muh-cookie))
              ("X-CSRF-Token" . ,csrf-token))

   :parser 'json-read
   :status-code '((204 . (lambda (&rest _) (message "Deleted item successfully")))
                  (500 . (lambda (&rest _) (message "Item doesn't seem to exist"))))))


(defun checkout (order)
  (setq order (=> orders basket-id))
  (request
   (concat "https://order.sweetgreen.com/api/line_items/" (=> order 'id))
   :type "PUT"
   :headers `(("Cookie" . ,(concat "_session_id=" muh-cookie))
              ("X-CSRF-Token" . ,csrf-token))
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
