import seaborn
import matplotlib.pyplot as plt
from mtspec import dpss
import os

tapers, lamb, theta = dpss(512, 2.5, 10)

output_folder = "_static"


plt.figure(figsize=(8, 4))
for i in range(5):
    plt.plot(tapers[:, i], lw=10, alpha=0.4)
plt.xlim(0, len(tapers[:, 0]))
plt.ylim(-0.08, 0.08)
plt.axis('off')
plt.text(x=0, y=1, s="mtspec", transform=plt.gca().transAxes, va="top",
         fontsize=130, fontweight=1000, color="0.3", variant="small-caps")


plt.subplots_adjust(left=0.02, right=0.98, bottom=0.02, top=0.98)
plt.savefig(os.path.join(output_folder, "logo.png"), transparent=True)
plt.savefig(os.path.join(output_folder, "logo.svg"), transparent=True)
plt.savefig(os.path.join(output_folder, "logo.pdf"), transparent=True)
