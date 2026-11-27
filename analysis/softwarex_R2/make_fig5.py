# Figure 5 (SoftwareX, R2): scenario experiment with the Pareto front, and model evaluation.
# Input: strategie_medie_pareto.csv (from verifica_scenario_pareto.R) and obs_sim.csv
# usage: python make_fig5.py <strategie_medie_pareto.csv> <obs_sim.csv> <out.png>
import sys, numpy as np, pandas as pd, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D

a = pd.read_csv(sys.argv[1]); o = pd.read_csv(sys.argv[2]); out = sys.argv[3]
a["pareto"] = a["pareto"].astype(str).str.upper().eq("TRUE")
f = a[a.pareto].copy(); dom = a[~a.pareto]
n01 = lambda x: (x - x.min()) / (x.max() - x.min())
Z = np.c_[n01(f["yield"]), n01(f.brix), n01(f.iwue), 1 - n01(f.irrigation), 1 - n01(f.events)]
f["dist"] = np.sqrt(((1 - Z) ** 2).sum(1))
picks = {"Highest yield": f.loc[f["yield"].idxmax()], "Highest IWUE": f.loc[f.iwue.idxmax()],
         "Compromise": f.loc[f.dist.idxmin()], "Highest Brix": f.loc[f.brix.idxmax()],
         "Lowest water use": f.loc[f.irrigation.idxmin()]}
cols = {0.6: "#dcdaee", 0.7: "#a39fd0", 0.8: "#6a51a3", 0.9: "#3f0f7d"}
GREY = "#e3e3e3"
plt.rcParams.update({"font.size": 11, "axes.spines.top": False, "axes.spines.right": False,
                     "axes.grid": True, "grid.color": "#e2e2e2", "axes.axisbelow": True})
fig = plt.figure(figsize=(9.08, 8.31), dpi=300)
gs = fig.add_gridspec(2, 2, width_ratios=[1.85, 1], left=0.085, right=0.985, bottom=0.07, top=0.86, hspace=0.33, wspace=0.30)

def strat(ax, ycol, ylab, off):
    ax.scatter(dom.irrigation, dom[ycol], s=30, c=GREY, edgecolors="#a8a8a8", linewidths=0.5, zorder=2)
    for lv, c in cols.items():
        d = f[np.isclose(f.reproductiveWS, lv)]
        ax.scatter(d.irrigation, d[ycol], s=30, c=c, edgecolors="#2b2b2b", linewidths=0.4, zorder=3)
    for lab, r in picks.items():
        ax.scatter(r.irrigation, r[ycol], marker="*", s=230, c="#fff6cf", edgecolors="black", linewidths=1.0, zorder=5)
        dx, dy, ha = off[lab]
        ax.annotate(lab, (r.irrigation, r[ycol]), xytext=(r.irrigation + dx, r[ycol] + dy), fontsize=10, ha=ha, va="center",
                    arrowprops=dict(arrowstyle="-", color="#777777", lw=0.8, shrinkA=1, shrinkB=7), zorder=6,
                    bbox=dict(boxstyle="round,pad=0.12", fc="white", ec="none", alpha=0.85))
    ax.set_xlabel("Seasonal irrigation (mm)"); ax.set_ylabel(ylab); ax.set_xlim(350, 534)

axA = fig.add_subplot(gs[0, 0])
strat(axA, "yield", "Simulated fruit yield (Mg ha$^{-1}$)",
      {"Highest yield": (-22, 5.5, "right"), "Highest IWUE": (-16, 5.5, "right"), "Compromise": (-16, 6.5, "right"),
       "Highest Brix": (18, -9, "left"), "Lowest water use": (22, -5.5, "left")})
axA.set_ylim(88, 146)
axC = fig.add_subplot(gs[1, 0])
strat(axC, "brix", "Simulated Brix (°)",
      {"Highest yield": (-6, -0.07, "right"), "Highest IWUE": (-22, -0.07, "right"), "Compromise": (26, -0.075, "left"),
       "Highest Brix": (22, 0.035, "left"), "Lowest water use": (-8, -0.065, "left")})
axC.set_ylim(5.29, 5.99)

def obs(ax, x, y, lim, ticks, unit):
    for sub, m, c, lab in [("calibrazione", "o", "#1f6fb5", "Calibration"), ("validazione", "^", "#d1661f", "Validation")]:
        d = o[o.subset == sub]; r2 = np.corrcoef(d[x], d[y])[0, 1] ** 2
        ax.scatter(d[x], d[y], marker=m, s=34, c=c, edgecolors="white", linewidths=0.5, zorder=3, label=f"{lab}  $R^2$ = {r2:.2f}")
    ax.plot(lim, lim, ls="--", c="#888888", lw=1, zorder=1)
    ax.set_xlim(lim); ax.set_ylim(lim); ax.set_xticks(ticks); ax.set_yticks(ticks); ax.set_aspect("equal")
    ax.set_xlabel(f"Observed ({unit})"); ax.set_ylabel(f"Simulated ({unit})")
    ax.legend(frameon=False, fontsize=10, loc="upper left", handletextpad=0.2, borderaxespad=0.2)
axB = fig.add_subplot(gs[0, 1]); obs(axB, "yield_ref", "yield", (0, 172), range(0, 161, 40), "Mg ha$^{-1}$")
axD = fig.add_subplot(gs[1, 1]); obs(axD, "brix_ref", "brix", (4, 8), range(4, 9), "°Brix")
for ax, t in [(axA, "A"), (axB, "B"), (axC, "C"), (axD, "D")]:
    ax.text(-0.085 if ax in (axA, axC) else -0.24, 1.035, t, transform=ax.transAxes, fontsize=15, fontweight="bold", va="bottom")

h = [Line2D([], [], marker="o", ls="", ms=6.5, mfc=c, mec="#2b2b2b", mew=0.4, label=f"{lv}") for lv, c in cols.items()]
h += [Line2D([], [], marker="o", ls="", ms=6.5, mfc=GREY, mec="#a8a8a8", mew=0.5, label="dominated strategy"),
      Line2D([], [], marker="*", ls="", ms=13, mfc="#fff6cf", mec="black", mew=1.0, label="strategy of Table 3")]
fig.legend(handles=h, loc="upper left", bbox_to_anchor=(0.07, 0.968), ncol=6, frameon=False, fontsize=10, handletextpad=0.1, columnspacing=1.0)
fig.text(0.082, 0.972, "Reproductive-phase $\\mathit{wsLevel}$ of the non-dominated strategies (Pareto front, $n$ = %d)" % len(f), fontsize=10, va="bottom")
h2 = [Line2D([], [], marker="o", ls="", ms=5.5, mfc="#1f6fb5", mec="white", label="Calibration ($n$ = %d)" % (o.subset == "calibrazione").sum()),
      Line2D([], [], marker="^", ls="", ms=5.5, mfc="#d1661f", mec="white", label="Validation ($n$ = %d)" % (o.subset == "validazione").sum()),
      Line2D([], [], ls="--", c="#888888", lw=1, label="1:1")]
fig.legend(handles=h2, loc="upper right", bbox_to_anchor=(0.99, 0.918), ncol=3, frameon=False, fontsize=10, handletextpad=0.3, columnspacing=1.0)
fig.savefig(out, dpi=300)
print("saved", out, "| front:", len(f), "| picks:", {k: (v.vegetativeWS, v.reproductiveWS, v.ripeningWS, int(v.transplantingDOY)) for k, v in picks.items()})
