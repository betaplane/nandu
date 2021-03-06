from IPython.core.pylabtools import print_figure
from IPython.core.display import publish_display_data
from matplotlib.figure import Figure
import os
get_ipython().run_line_magic('matplotlib', 'inline')

def save_print(fig):
    try:
        # from IPython.core.debugger import Tracer; Tracer()()
        base_dir, buffer = os.path.split(buffer_file_name())
        name = savefig()
        if name.lower() is not "none":
            path = os.path.realpath(os.path.join(base_dir, resources_dir,
                             os.path.splitext(buffer)[0], name))
            # the ``None`` kwargs force values to be taken from the default style,
            # so they can be easily set in style sheets
            fig.savefig(path, dpi=None, facecolor=None, edgecolor=None, bbox_inches=None)
            return publish_display_data({'text/org': '[[{}]]'.format(path)})
    except NameError: # might not be defined
        return print_figure(fig, 'png', bbox_inches='tight')

png = get_ipython().display_formatter.formatters['image/png']
png.for_type(Figure, save_print)
