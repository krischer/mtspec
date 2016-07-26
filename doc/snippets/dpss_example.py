import matplotlib.pyplot as plt
plt.style.use("ggplot")

from mtspec import dpss

tapers, _, _ = dpss(npts=512, fw=2.5, number_of_tapers=8)

for i in range(8):
    plt.plot(tapers[:, i])
plt.xlim(0, len(tapers[:, 0]))

plt.tight_layout()
plt.show()
