import matplotlib.pyplot as plt
import os

get_ipython().run_line_magic('matplotlib', 'inline')
get_ipython().run_line_magic('config', "InlineBackend.figure_formats = ['png', 'pdf']")

plt.style.use(['default',
    os.path.join(os.path.realpath(os.path.dirname(__file__)),
                 'mpl_configdir', 'nandu_dark.mplstyle')
    ])
