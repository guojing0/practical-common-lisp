(defvar *account-numbers* 0)

;;; generic functions

(defgeneric customer-name (account)) ; reader for customer-name
(defgeneric (setf customer-name) (value account)) ; writer for customer-name

(defgeneric withdraw (account amount)
  (:documentation "Withdraw the specified amount from the account.
Signal an error if the current balance is less than amount."))

(defgeneric assess-low-balance-penality (account))
(defgeneric merge-accounts (account1 account2))

;;; actual implementations

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a name."))
   (balance
    :initarg :balance
    :initform 0
    :accessor balance ; :accessor is the combination of :reader and :writer
    :documentation "Current balance")
   (account-number
    :initform (incf *account-numbers*))
   (account-type ; the following can be combined to :accessor account-type
    :reader account-type
    :writer (setf account-type))))

(defmethod customer-name ((account bank-account))
  (slot-value account 'customer-name))

(defmethod (setf customer-name) (value (account bank-account))
  (setf (slot-value account 'customer-name) value))

(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "Account overdrawn."))
  (decf (balance account) amount))

;; (overdraft-account checking-account) -> bank-account object

;(defmethod withdraw :before ((account checking-account) amount)
;  (let ((overdraft (- amount (balance account))))
;    (when (plusp overdraft)
;      (withdraw (overdraft-account account) overdraft)
;      (incf (balance account) overdraft))))

;(defmethod withdraw ((account (eql *account-of-bank-president*)) amount)
;  (let ((overdraft (- amount (balance account))))
;    (when (plusp overdraft)
;      (incf (balance account) (embezzle *bank* overdraft)))
;    (call-next-method)))

(defmethod initialize-instance :after ((account bank-account)
				       &key opening-bonus-per)
  (with-slots (balance) account
    ;; if opening bonus per, then calculate the overall balance
    (when opening-bonus-per
      (incf balance (* balance (/ opening-bonus-per 100))))
    
    ;; determine the account type
    (setf (slot-value account 'account-type)
	  (cond
	    ((>= balance 100000) :gold)
	    ((>= balance 50000)  :silver)
	    (t                   :bronze)))))

(defmethod assess-low-balance-penality ((account bank-account))
  (with-slots (balance) account ; OR (with-accessors ((balance balance)) account
    (when (< (balance account) 500)
      (decf balance (* balance 0.01)))))

(defmethod merge-accounts ((account1 bank-account)
			   (account2 bank-account))
  (with-accessors ((balance1 balance)) account1
    (with-accessors ((balance2 balance)) account2
      (incf balance1 balance2)
      (setf balance2 0))))
