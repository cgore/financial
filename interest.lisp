;;;; Copyright (c) 2011, Christopher Mark Gore,
;;;; All rights reserved.
;;;;
;;;; 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(load "utilities")
(use-package 'utilities)

(defun tvm-fv (payment interest-rate payments-per-period periods)
  "Calculates the time value of money (TVM) future value (FV).  This function
  implicitly assumes an annual compounding period."
  (assert (numberp payment))
  (assert (numberp interest-rate))
  (assert (positive-integer? payments-per-period))
  (assert (positive-integer? periods))
  (apply #'+
         (mapcar (lambda (period)
                   (* payments-per-period (float payment)
                      (expt (float interest-rate) period)))
                 (loop for i from 1 to periods collect i))))

(defun tvm-perpetual (payment interest-rate payments-per-period periods)
  "Calculates the final TVM FV perpetual return.  That is, how much we can
  expect to see in returns forever from a TVM-FV payment schedule after the
  payments have been completed."
  (assert (numberp payment))
  (assert (numberp interest-rate))
  (assert (positive-integer? payments-per-period))
  (assert (positive-integer? periods))
  (* (1- interest-rate)
     (tvm-fv payment interest-rate payments-per-period periods)))

(defun tvm-fv-payment (desired-fv interest-rate payments-per-period periods)
  "Calculates what payment is required to generate the desired future value
  given the specified payments per year, annual interest rate, and number of
  periods.  This function implicitly assumes an annual compounding period."
  (assert (numberp desired-fv))
  (assert (numberp interest-rate))
  (assert (positive-integer? payments-per-period))
  (assert (positive-integer? periods))
  (let ((payment 1.00))
    (while (< (tvm-fv payment interest-rate payments-per-period periods)
              desired-fv)
      (incf payment))
    payment))

(defun tvm-perpetuity-payment
  (desired-perpetuity interest-rate payments-per-period periods)
  "Calculates what payment is required to generate the desired perpetuity given
  the specified payment schedule and interest rate."
  (assert (numberp desired-perpetuity))
  (assert (numberp interest-rate))
  (assert (positive-integer? payments-per-period))
  (assert (positive-integer? periods))
  ;;; NB: This algorithm is horrible.  Aren't multi-gigahertz CPUs great?
  (let ((payment 1.00))
    (while (< (tvm-perpetual payment interest-rate payments-per-period periods)
              desired-perpetuity)
      (incf payment))
    payment))
