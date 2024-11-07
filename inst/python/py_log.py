# This file is part of the R package "aifeducation".
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as published by
# the Free Software Foundation.
#
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>

import csv
import time

def write_log_py(log_file, value_top = 0, value_middle = 0, value_bottom = 0,
total_top = 1, total_middle = 1, total_bottom = 1, message_top = "NA", message_middle = "NA",
message_bottom = "NA", last_log = None, write_interval = 2):

  log_time=None
  
  if not (log_file is None):
    if not (last_log is None):
      diff=time.time()-last_log
    else:
      diff=float("inf")
      
    if diff>write_interval:
      try:
        f = open(log_file,"w",newline="")
        fieldnames = ["value", "total","message"]
        writer = csv.DictWriter(f, fieldnames=fieldnames,dialect='unix')
        writer.writeheader()
        writer.writerow({'value': value_top, 'total': total_top, 'message': message_top})
        writer.writerow({'value': value_middle, 'total': total_middle, 'message': message_middle})
        writer.writerow({'value': value_bottom, 'total': total_bottom, 'message': message_bottom})
        log_time=time.time()
        f.close()
      except:
        log_time=last_log
    return log_time
  else:
    return None

def write_log_performance_py(log_file, history, last_log = None, write_interval = 2):
  
  log_time=None
  
  if not (log_file is None):
    if not (last_log is None):
      diff=time.time()-last_log
    else:
      diff=float("inf")
        
    if diff>write_interval:
      try:
        f = open(log_file,"w",newline="")
        writer = csv.writer(f, dialect='unix')
        writer.writerows(history)
        log_time=time.time()
        f.close()
      except:
        log_time=last_log
    return log_time
  else:
    return None


