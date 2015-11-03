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
(defvar le-orders '())
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
                (setq basket-id (number-to-string basket_id))
                (print (basket_id . order))
                (push `(,basket_id . ,order) le-orders)))))

(setq order-id nil)
(setq basket-id nil)

(=> (aref (assoc-default "something" orders) 0) 'basket_id)
(number-to-string (=> orders 642731 'ignored_order_id))

(setq holi '(("hola" . "tuma")
             ("heyo" . "lelel")))
(add-to-list 'holi `(,(car (assoc "hola" holi)) . "asdfasd"))
