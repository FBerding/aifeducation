def batch_iterator(dataset, batch_size = 200, report_to_shiny_app = False):
    for i in range(0, len(dataset), batch_size):
        if report_to_shiny_app:
            r.py_update_aifeducation_progress_bar_steps(
                value = min(i + batch_size, len(dataset)),
                total = len(dataset),
                title = ("Documents: " + str(min(i + batch_size, len(dataset))) + "/" + str(len(dataset))))
        yield dataset[i : i + batch_size]["text"]
