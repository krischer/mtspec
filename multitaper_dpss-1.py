# Same as the code snippet in the docstring, just a bit prettier.
import matplotlib.pyplot as plt
plt.style.use("ggplot")
from mtspec import dpss
tapers, lamb, theta = dpss(512, 2.5, 5)
for i in range(5):
    plt.plot(tapers[:, i])
plt.xlim(0, 512)
plt.ylim(-0.09, 0.09)
plt.tight_layout()