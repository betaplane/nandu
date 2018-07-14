from IPython.core.pylabtools import print_figure
from matplotlib.figure import Figure

def save_print(fig):
    fig.savefig('test.png')
    return print_figure(fig, 'png')

png = get_ipython().display_formatter.formatters['image/png']
png.for_type(Figure, save_print)
