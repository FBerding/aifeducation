def batch_iterator(dataset, batch_size = 200,
                   log_file = None, write_interval = 2,
                   value_top = 0, total_top = 1, message_top = "NA"):
    last_log = None
    dataset_len = len(dataset)
    
    for i in range(0, dataset_len, batch_size):
        value_middle = min(i + batch_size, dataset_len)
        if value_middle == dataset_len: # this is the last iteration
          last_log = None
        last_log = write_log_py(log_file,
                                value_top = value_top, total_top = total_top, message_top = message_top,
                                value_middle = value_middle,
                                total_middle = dataset_len,
                                message_middle = "Documents",
                                last_log = last_log, write_interval = write_interval)
        yield dataset[i : i + batch_size]["text"]
