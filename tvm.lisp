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

(defun future-value-formula (present-value future-value interest% periods)
  (let ((arguments (list present-value future-value interest% periods))
        (interest-rate (when interest% (1+ (/ interest% 100.0)))))
    (assert (= 1 (count nil arguments)))
    (mapcar (lambda (x)
              (assert (or (numberp x) (null x))))
            arguments)
    (cond ((null present-value)
           (/ future-value (expt interest-rate periods)))
          ((null future-value)
           (* present-value (expt interest-rate periods)))
          ((null interest%)
           (* 100.0 (1- (expt (/ future-value present-value) (/ periods)))))
          ((null periods)
           (log (/ future-value present-value) interest-rate))
          (t (error "over-specified TVM system (also, s.b. unreachable)")))))

;;; Numbers compared against an HP 10bII.
(assert (= (future-value-formula nil 150000.00 5.0 30) 34706.664))
(assert (= (future-value-formula 10000.00 nil 5.0 30) 43219.367))
(assert (= (future-value-formula 10000.00 50000.00 nil 30) 5.5113077))
(assert (= (future-value-formula 5000.00 10000.00 5.75 nil) 12.398077))
