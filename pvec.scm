(define-module pvector
  (export
     pvector?
     pvector
     pvector-length
     pvector-cons
     pvector-ref
     pvector-update
     list->pvector
     pvector->list
   ))

(select-module pvector)


;;; vector utilities

(define (update-vector vec i val)
  (let ((tr (vector-copy vec)))
    (vector-set! tr i val)
    tr))

(define (expand-vector vec val)
  (let* ((length (vector-length vec))
	 (tr (vector-copy vec 0 (+ 1 length) #f)))
    (vector-set! tr length val)
    tr))

(define (make-vector-chain shift val)
  (if (= shift 5)
      (vector val)
      (vector (make-vector-chain (- shift 5) val))))


;;; pvector

(define-class <pvector> ()
  ((size :init-keyword :size)
   (shift :init-keyword :shift)
   (root :init-keyword :root)))


(define (pvector? x)
  (is-a? x <pvector>))


(define pvector-null (make <pvector> :size 0 :shift 0 :root #()))


(define (pvector . args)
  (fold pvector-cons pvector-null args))


(define (pvector-length pvec)
  (slot-ref pvec 'size))


(define (pvector-update pvec i val)
  (let ((shift (slot-ref pvec 'shift)))
      (make <pvector>
	:size (slot-ref pvec 'size)
	:shift shift
	:root (pvector-update* shift
			 (slot-ref pvec 'root)
			 i
			 val))))


(define (pvector-update* shift vec i val)
  (if (zero? shift)
      (update-vector vec (logand i #x01f) val)
      (let* ((subidx (logand (ash i (- shift)) #x01f))
	     (child (pvector-update* (- shift 5) (vector-ref vec subidx) i val)))
	(update-vector vec subidx child))))


(define (pvector-ref pvec i)
  (unless (< -1 i (slot-ref pvec 'size))
	  (error "pvector-ref index out of range:" i))
  (let loop ((shift (slot-ref pvec 'shift))
	     (arr   (slot-ref pvec 'root)))
    (if (zero? shift)
	(vector-ref arr (logand i #x01f))
	(loop (- shift 5) (vector-ref arr (logand (ash i (- shift)) #x01f))))))


(define (pvector-cons obj pvec)
  (let* ((shift (slot-ref pvec 'shift))
	 (size  (slot-ref pvec 'size))
	 (root  (slot-ref pvec 'root))
	 (new-root (pvector-cons* shift root obj)))
    (if new-root
	(make <pvector>
	  :size (+ size 1)
	  :shift shift
	  :root new-root)
	(make <pvector>
	  :size (+ size 1)
	  :shift (+ shift 5)
	  :root (vector root (make-vector-chain (+ shift 5) obj))))))

(define (pvector-cons* shift vec val)
  (if (zero? shift)
      (pvector-cons-leaf vec val)
      (pvector-cons-node shift vec val)))


(define (node-full? vec)
  (= (vector-length vec) 32))

(define (pvector-cons-leaf vec val)
  (if (node-full? vec)
      #f
      (expand-vector vec val)))

(define (pvector-cons-node shift vec val)
  (let ((child (pvector-cons* (- shift 5)
			 (vector-ref vec (- (vector-length vec) 1))
			 val)))
    (if child
	(update-vector vec (- (vector-length vec) 1) child)
	(if (node-full? vec)
	    #f
	    (expand-vector vec (make-vector-chain shift val))))))


(define (list->pvector list)
  (apply pvector list))

(define (pvector->list pvec)
  (reverse (pvector->list* pvec)))

(define (pvector->list* pvec)
  (let loop ((acc '()) (index 0))
    (if (= index (pvector-length pvec))
	acc
	(loop (cons (pvector-ref pvec index) acc) (+ index 1)))))



