;;; mk-calendar.el --- Calendar settings -*- lexical-binding: t; -*-
;;;
;;; Copyright © 2015 Mark Karpov <markkarpov@opmbx.org>
;;;
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Fortunately, I'm living in Russia, so let's make calendar appear
;;; Russian.

;;; Code:

(setq calendar-week-start-day 1 ; Понедельник
      calendar-day-name-array ["Воскресенье" "Понедельник" "Вторник"
                               "Среда" "Четверг" "Пятница" "Суббота"]
      calendar-day-header-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб" "Вс"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
                                 "Июнь" "Июль" "Август" "Сентябрь"
                                 "Октябрь" "Ноябрь" "Декабрь"])

(provide 'mk-calendar)

;;; mk-calendar.el ends here
