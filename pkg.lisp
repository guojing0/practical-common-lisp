(defpackage :org.guoj.email-db
  (:use :common-lisp
	:org.guoj.text-db
	:com.acme.text)
  (:import-from :com.acme.email :parse-email-address)
  (:shadow :build-index)
  (:shadow-import-from :org.guoj.text-db :save))(

(in-package :org.guoj.email-db)
