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

def batch_iterator(dataset, batch_size = 200, report_to_shiny_app = False):
    for i in range(0, len(dataset), batch_size):
        if report_to_shiny_app:
            r.py_update_aifeducation_progress_bar_steps(
                value = min(i + batch_size, len(dataset)),
                total = len(dataset),
                title = ("Documents: " + str(min(i + batch_size, len(dataset))) + "/" + str(len(dataset))))
        yield dataset[i : i + batch_size]["text"]
