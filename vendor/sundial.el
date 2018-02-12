;;; sundial.el -- A powerful means to trigger events at sunrise/sunset.
;;
;; Author:
;;     Name: Maniroth Ouk
;;     Email: maniroth_ouk@outlook.com
;; Last Updated: <12 Feb. 2018 -- 14:42 (SE Asia Standard Time) by Maniroth Ouk>
;; License: MIT
;;
;;; Commentary:
;;
;; This is a fork of the ``theme-changer'' package by ``Joshua B. Griffith''.
;; Instead of focusing on *just* switching themes at the sunrise/sunset events,
;; we can trigger `hooks' instead, thus, allowing for more extensiblity.
;;
;; I have been hacking away at ``theme-changer'' using emacs' built-in
;; *advice* system and it has been a *cheap* solution to my needs, but this
;; recent version (the one that I am forking right now) broke my cheap
;; solution. I attempt to remedy my needs for a terse event-timer for
;; sunrise/sunset and fix my broken `init.el' with this fork.
;;
;; The original software had used the same language for its software license
;; as that of the MIT license, which is the main license in use by the rest
;; of my emacs config. Thus, this fork, in alignment with the original
;; software and in conjunction with the rest of my emacs config, will remain
;; under the MIT license.
;;
;;
;; The usage of `sundial.el' is as follows:
;;
;; ;; Starts off the cascading sundial event-timer.
;; (add-hook 'sundial-daytime-hook #'some-function)
;; (add-hook 'sundial-nighttime-hook #'some-other-function)
;; (sundial-start)
;;
;; ;; Can even add functions to hooks even after the event-timer is in play.
;; ;; However, the new functions on the hooks will only run (take effect) after
;; ;; the next sunrise and/or sunset event occurs.
;; (add-hook 'sundial-daytime-hook #'another-function)
;;
;;
;; CAUTION!
;; `sundial.el' is not immune to conflicting setting(s) of
;; `calendar-longitude' and/or `calendar-latitude' with the local timezone
;; of the OS. In the event, this does happen, the functions from `solar.el'
;; (the basis for calculating the sunrise/sunset) will report that the
;; *current day* will either have no sunrise or sunset (or even report that
;; both will not occur). Furthermore, we assume that the user is using this
;; package in a location with both sunrise and sunset.
;;
;;
;;; Original Software:
;; ;;; theme-changer.el --- Sunrise/Sunset Theme Changer for Emacs
;;
;; ;; Copyright (C) 2011-2013 Joshua B. Griffith
;;
;; ;; Author: Joshua B. Griffith <josh.griffith@gmail.com>
;; ;; Contributors: Joe Snikeris, Mike Fisher, Göktuğ Kayaalp
;; ;; URL: https://github.com/hadronzoo/theme-changer
;; ;; Created: 20 Jun 2011
;; ;; Version: 2.1.0
;; ;; Keywords: color-theme, deftheme, solar, sunrise, sunset
;;
;; ;; Permission is hereby granted, free of charge, to any person obtaining
;; ;; a copy of this software and associated documentation files (the
;; ;; "Software"), to deal in the Software without restriction, including
;; ;; without limitation the rights to use, copy, modify, merge, publish,
;; ;; distribute, sublicense, and/or sell copies of the Software, and to
;; ;; permit persons to whom the Software is furnished to do so, subject to
;; ;; the following conditions:
;;
;; ;; The above copyright notice and this permission notice shall be
;; ;; included in all copies or substantial portions of the Software.
;;
;; ;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; ;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; ;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; ;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; ;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; ;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; ;; SOFTWARE.
;;
;; ;;; Commentary:
;;
;; ;; Given a location and day/night color themes, this file provides a
;; ;; `change-theme` function that selects the appropriate theme based on
;; ;; whether it is day or night.  It will continue to change themes at
;; ;; sunrise and sunset.  To install:
;;
;; ;; Set the location:
;; ;;     (setq calendar-location-name "Dallas, TX")
;; ;;     (setq calendar-latitude 32.85)
;; ;;     (setq calendar-longitude -96.85)
;;
;; ;; Specify the day and night themes:
;; ;;     (require 'theme-changer)
;; ;;     (change-theme 'solarized-light 'solarized-dark)
;;
;; ;; If you specify a list of themes, a random one will be chosen at
;; ;; each change:
;; ;;     (change-theme '(solarized-light sanityinc-tomorrow-day)
;; ;;                   '(solarized-dark sanityinc-tomorrow-night))
;;
;; ;; You can also pass nil as either of parameters to change-theme, with the
;; ;; effect of not using a theme (or using the default Emacs theme) during that
;; ;; period of the day.  For example:
;;
;; ;;     (change-theme nil 'solarized-dark)
;;
;; ;; will result in setting the default Emacs theme during the day, and
;; ;; solarized-dark during the night.
;;
;; ;; You may need to add this file path to your loadpath.  For example:
;; ;;     (add-to-list 'load-path "~/.emacs.d/elisp/theme-changer")
;;
;; ;; If you want to use the color-theme package instead of the Emacs 24 color
;; ;; theme facility:
;; ;;     (setq theme-changer-mode "color-theme")
;; ;;     (change-theme 'color-theme-solarized-light 'color-theme-solarized-dark)
;;
;; ;;; Code:
;;
;; (eval-when-compile
;;   (require 'cl))
;; (require 'solar)
;;
;; (defvar theme-changer-mode "deftheme"
;;   "Specify the theme change mode: \"color-theme\" or Emacs 24's \"deftheme\".")
;;
;; (defun theme-changer-hour-fraction-to-time (date hour-fraction)
;;   (let*
;;       ((now (decode-time (current-time)))
;;
;;        (month (first   date))
;;        (day   (second  date))
;;        (year  (third   date))
;;        (zone  (ninth   now))
;;
;;        (frac-hour (cl-truncate hour-fraction))
;;        (hour (first frac-hour))
;;
;;        (frac-minutes (cl-truncate (* (second frac-hour) 60)))
;;        (minute (first frac-minutes))
;;
;;        (frac-seconds (cl-truncate (* (second frac-minutes) 60)))
;;        (sec (first frac-seconds)))
;;     (encode-time sec minute hour day month year zone)))
;;
;;
;; (defun theme-changer-sunrise-sunset-times (date)
;;   (let*
;;       ((l (solar-sunrise-sunset date))
;;        (sunrise-time (theme-changer-hour-fraction-to-time date (caar l)))
;;        (sunset-time (theme-changer-hour-fraction-to-time date (caadr l))))
;;     (list sunrise-time sunset-time)))
;;
;; (defun theme-changer-today () (calendar-current-date))
;;
;; (defun theme-changer-tomorrow ()
;;   (calendar-gregorian-from-absolute
;;    (+ 1 (calendar-absolute-from-gregorian (theme-changer-today)))))
;;
;; (defun theme-changer-add-second (time)
;;   (time-add time (seconds-to-time 1)))
;;
;; (defun theme-changer-switch-theme (old new)
;;   "Change the theme from OLD to NEW.
;;
;; Uses Emacs 24's built-in theme facility (\"deftheme\") or
;; color-theme, depending on THEME-CHANGER-MODE.
;;
;; NEW may be a list of themes, in which case a random theme is
;; chosen from that list.
;;
;; If NEW is set to nil, shall switch to default Emacs theme.
;;
;; Returns the theme that was enabled."
;;   (let ((new (if (listp new)
;;                  (elt new (random (length new)))
;;                new))
;;         (enable (if (not (string= theme-changer-mode "deftheme"))
;;                     (lambda () (apply (symbol-function new) '()))
;;                   (lambda () (load-theme new t)))))
;;     (disable-theme old)
;;     (if new (funcall enable))
;;     new))
;;
;; (defun change-theme (day-theme night-theme &optional old-theme)
;;   (let* ((now (current-time))
;;          (sunrise-tomorrow (first (theme-changer-sunrise-sunset-times
;;                                    (theme-changer-tomorrow)))))
;;     (destructuring-bind (sunrise-today sunset-today)
;;         (theme-changer-sunrise-sunset-times (theme-changer-today))
;;       (destructuring-bind (next-change . theme)
;;           (cond ((time-less-p now sunrise-today)
;;                  (cons sunrise-today night-theme))
;;                 ((time-less-p now sunset-today)
;;                  (cons sunset-today day-theme))
;;                 (t (cons sunrise-tomorrow night-theme)))
;;         (let ((old-theme (theme-changer-switch-theme old-theme theme)))
;;           (run-at-time (theme-changer-add-second next-change) nil
;;                        'change-theme day-theme night-theme old-theme))))))
;;
;; (provide 'theme-changer)
;;
;; ;;; theme-changer.el ends here
;;
;;; End of Original Software:
;;
;;; Code:

(require 'solar)

(defvar sundial-daytime-hook nil
  "A hook that runs at the *around* the time of local sunrise
\(or at the local time of the sunrise for the location set under
`calendar-latitude' and `calendar-longitude').

The accuracy of the time of sunrise and sunset is wholly determined
by the variable `solar-error' and calculating functions
\(i.e. `solar-sunrise-sunset') defined in the file `solar.el'.")

(defvar sundial-nighttime-hook nil
  "A hook that runs at the *around* the time of local sunset
\(or at the local time of the sunset for the location set under
`calendar-latitude' and `calendar-longitude').

The accuracy of the time of sunrise and sunset is wholly determined
by the variable `solar-error' and calculating functions
\(i.e. `solar-sunrise-sunset') defined in the file `solar.el'.")

(defun sundial--decimal-to-print-time (time)
  "Takes the time in the format HH.ff* and returns in the form HH:MM (24-hour format).

This is the inverse of the function `sundial--print-to-decimal-time'."
  (solar-daylight time))

(defun sundial--print-to-decimal-time (time)
  "Takes the time in the format HH:MM and returns in the form HH.ff* (24-hour format).

This is the inverse of the function `sundial--decimal-to-print-time'."
  (let ((temp (mapcar 'string-to-number (split-string time ":"))))
    (+ (nth 0 temp)
       (/ (nth 1 temp) 60.0))))

(defun sundial--compare-time-strings (t1 t2)
  "Compares time T1 to time T2, where the times given are strings of form HH:MM.

A return value of
-1 indicates that T1 is before T2;
 0 indicates that T1 is the same time as T2;
 1 indicates that T1 is after T2.

This function assumes that T1 and T2 follow the format of HH:MM in 24-hour
format."
  (let* ((comparable-t1 (sundial--print-to-decimal-time t1))
         (comparable-t2 (sundial--print-to-decimal-time t2)))
    (cond ((< comparable-t1 comparable-t2) -1)
          ((> comparable-t1 comparable-t2)  1)
          (t 0))))

(defun sundial--time-less-p (t1 t2)
  "Returns t if and only if time T1 is before time T2."
  (= -1 (sundial--compare-time-strings t1 t2)))

(defun sundial-start ()
  "Triggers the start of the sundial, thus, starting the ability to
trigger events at sunrise and/or sunset."
  (let* ((full-time-list (split-string (format-time-string "%m,%d,%Y,%H:%M") ","))
         (today          (mapcar 'string-to-number (butlast full-time-list)))
         (current-time   (car (last full-time-list)))
         (tomorrow       (calendar-gregorian-from-absolute
                          (+ 1 (calendar-absolute-from-gregorian today))))

         (sunrise-sunset-today    (solar-sunrise-sunset today))
         (sunrise-sunset-tomorrow (solar-sunrise-sunset tomorrow))

         (maybe-sunrise-now   (car (nth 0 sunrise-sunset-today)))
         (maybe-sunset-now    (car (nth 1 sunrise-sunset-today)))
         (maybe-sunrise-later (car (nth 0 sunrise-sunset-tomorrow))))
    (unless maybe-sunrise-now
      (error "%s" "Sunrise time is missing (nil).
Could indicate that the location set through variables do not reflect current location."))
    (unless maybe-sunset-now
      (error "%s" "Sunset time is missing (nil).
Could indicate that the location set through variables do not reflect current location."))

    (let ((sunrise-now   (sundial--decimal-to-print-time maybe-sunrise-now))
          (sunset-now    (sundial--decimal-to-print-time maybe-sunset-now))
          (sunrise-later (sundial--decimal-to-print-time maybe-sunrise-later)))
      (cond ((sundial--time-less-p current-time sunrise-now)
             (prog1
                 ;; Between 0AM and sunrise; still night
                 (run-hooks 'sundial-nighttime-hook)
               (run-at-time sunrise-now nil #'sundial-start)))
            ((sundial--time-less-p current-time sunset-now)
             (prog1
                 ;; Between sunrise and sunset; still day
                 (run-hooks 'sundial-daytime-hook)
               (run-at-time sunset-now nil #'sundial-start)))
            (t
             (prog1
                 ;; Thus, before tomorrow's sunrise at today's nighttime
                 (run-hooks 'sundial-nighttime-hook)
               (let* ((tomorrow-time-split   (split-string sunrise-later ":"))
                      (tomorrow-hour         (nth 0 tomorrow-time-split))
                      (tomorrow-min          (nth 1 tomorrow-time-split))
                      (tomorrow-execute-time (encode-time 0
                                                          tomorrow-min
                                                          tomorrow-hour
                                                          (nth 1 tomorrow)
                                                          (nth 0 tomorrow)
                                                          (nth 2 tomorrow))))
                 (run-at-time tomorrow-execute-time nil #'sundial-start))))))))

(provide 'sundial)

;; Local Variables:
;; indent-tabs-mode: nil
;; time-stamp-pattern: "16/Last Updated:[ \t]+\\\\?[\"<]+%02d %3b. %:y -- %02H:%02M (%Z) by %U\\\\?[\">]"
;; eval: (add-hook 'before-save-hook 'time-stamp nil t)
;; End:
