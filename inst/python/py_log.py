import csv
import time

def write_log_py(log_file, value_top = 0, value_middle = 0, value_bottom = 0,
total_top = 1, total_middle = 1, total_bottom = 1, message_top = "NA", message_middle = "NA",
message_bottom = "NA", last_log = None, write_interval = 2):

  log_time=None
  if not (log_file is None):
    if value_top==total_top or value_middle==total_middle or value_bottom==total_bottom or value_top==1 or value_middle==1 or value_bottom==1:
      try:
        f = open(file=log_file,mode="w",newline="")
        fieldnames = ["value", "total","message"]
        writer = csv.DictWriter(f, fieldnames=fieldnames,dialect='unix')
        writer.writeheader()
        writer.writerow({'value': value_top, 'total': total_top, 'message': message_top})
        writer.writerow({'value': value_middle, 'total': total_middle, 'message': message_middle})
        writer.writerow({'value': value_bottom, 'total': total_bottom, 'message': message_bottom})
        log_time=time.time()
      except:
        log_time=last_log
      finally:
        f.close()
    else:
      if not (last_log is None):
        diff=time.time()-last_log
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
          except:
            log_time=last_log
          finally:
            f.close()
    return log_time
  else:
    return None

def write_log_performance_py(log_file, history, last_log = None, write_interval = 2):
  log_time=None
  if not (log_file is None):
      if not (last_log is None):
        diff=time.time()-last_log
        if diff>write_interval:
          try:
            f = open(log_file,"w",newline="")
            writer = csv.writer(f, dialect='unix')
            writer.writerows(history)
            log_time=time.time()
          except:
            log_time=last_log
          finally:
            f.close()
        else:
          log_time=last_log
      else:
        try:
          f = open(log_file,"w",newline="")
          writer = csv.writer(f, dialect='unix')
          writer.writerows(history)
          log_time=time.time()
        except:
          log_time=last_log
        finally:
          f.close()
      return log_time
  else:
      return None


