"""
Policy brief 2×2 plot: SWE and Runoff for Amu Darya and Syr Darya.
SSP5-8.5 only. ggplot styling with grey banners.
"""

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import numpy as np
from matplotlib.lines import Line2D
from matplotlib.patches import FancyBboxPatch

# --- Config ---
basins = ["AMU_DARYA", "SYR_DARYA"]
basin_labels = {"AMU_DARYA": "Amu Darya Basin", "SYR_DARYA": "Syr Darya Basin"}

variables = {
    "SWE": ("../../data/processed_SWE.csv", "Snow Water Equivalent (mm)"),
    "Rof": ("../../data/processed_Rof.csv", "Snowmelt Runoff (mm)"),
}
var_display = {"SWE": "Snow Water Equivalent", "Rof": "Snowmelt Runoff"}

ssp = "ssp585"

# Viridis discrete colors for 6 elevation bands
viridis = plt.get_cmap("viridis", 6)
elev_bands_sorted = ["0-1000", "1000-2000", "2000-3000", "3000-4000", "4000-5000", "5000-6000"]
elev_colors = {band: viridis(i) for i, band in enumerate(elev_bands_sorted)}
elev_labels = {band: f"{band.split('-')[0]}–{band.split('-')[1]} m" for band in elev_bands_sorted}

# --- Load data ---
data = {}
for vname, (path, _) in variables.items():
    df = pd.read_csv(path)
    df["elev_band"] = df["elev_low"].astype(int).astype(str) + "-" + df["elev_high"].astype(int).astype(str)
    df["hydro_date"] = pd.to_datetime("2000-09-01") + pd.to_timedelta(df["hydro_day"] - 1, unit="D")
    data[vname] = df

# --- ggplot style ---
plt.rcParams.update({
    "axes.facecolor": "#EBEBEB",
    "figure.facecolor": "white",
    "axes.edgecolor": "white",
    "axes.grid": True,
    "grid.color": "white",
    "grid.linewidth": 1.0,
    "xtick.color": "#333333",
    "ytick.color": "#333333",
    "text.color": "#333333",
    "axes.labelcolor": "#333333",
    "xtick.direction": "out",
    "ytick.direction": "out",
})

# --- Layout ---
fig = plt.figure(figsize=(10, 8), facecolor="white")

left_margin = 0.08
top_margin = 0.09
right_edge = 0.96
bottom_edge = 0.14
plot_w = (right_edge - left_margin) / 2
plot_h = (1.0 - top_margin - bottom_edge) / 2

for row, vname in enumerate(variables):
    _, ylabel = variables[vname]
    for col, basin in enumerate(basins):
        x0 = left_margin + col * plot_w + 0.02
        y0 = (1.0 - top_margin) - (row + 1) * plot_h + 0.02
        ax = fig.add_axes([x0, y0, plot_w - 0.04, plot_h - 0.04])

        df_sub = data[vname][(data[vname]["region"] == basin) & (data[vname]["scenario"] == ssp)]

        for elev_band in elev_bands_sorted:
            df_elev = df_sub[df_sub["elev_band"] == elev_band].sort_values("hydro_day")
            if df_elev.empty:
                continue
            color = elev_colors[elev_band]
            ax.plot(df_elev["hydro_date"], df_elev["historical_mean"],
                    color=color, linestyle="-", linewidth=1.8)
            ax.plot(df_elev["hydro_date"], df_elev["future_mean"],
                    color=color, linestyle="--", linewidth=1.8)

        ax.xaxis.set_major_locator(mdates.MonthLocator())
        ax.xaxis.set_major_formatter(mdates.DateFormatter("%b"))
        ax.tick_params(axis="both", labelsize=10)
        ax.set_xlim(pd.Timestamp("2000-09-01"), pd.Timestamp("2001-08-31"))

        if col == 0:
            ax.set_ylabel(ylabel, fontsize=11, fontweight="bold")
            ax.yaxis.set_label_coords(-0.15, 0.5)

        if row == 1:
            plt.setp(ax.get_xticklabels(), rotation=45, ha="right", fontsize=10)
        else:
            ax.set_xticklabels([])
            ax.tick_params(axis="x", length=0)

# --- Grey basin banners across the top ---
for col, basin in enumerate(basins):
    x0 = left_margin + col * plot_w + 0.02
    banner_w = plot_w - 0.04
    rect = FancyBboxPatch((x0, 1.0 - top_margin + 0.01), banner_w, 0.05,
                           boxstyle="round,pad=0.005",
                           facecolor="#4a4a4a", edgecolor="none",
                           transform=fig.transFigure, clip_on=False)
    fig.add_artist(rect)
    fig.text(x0 + banner_w / 2, 1.0 - top_margin + 0.035,
             basin_labels[basin], ha="center", va="center",
             fontsize=13, fontweight="bold", color="white",
             transform=fig.transFigure)

# --- Remove left margin space since no banners ---

# --- Two-line legend matching original R style ---
# Line 1: Elevation Band
elev_handles = []
for band in elev_bands_sorted:
    elev_handles.append(Line2D([0], [0], color=elev_colors[band], lw=3,
                                label=elev_labels[band]))

leg1 = fig.legend(handles=elev_handles, loc="lower center",
                   ncol=6, fontsize=9, frameon=False,
                   bbox_to_anchor=(0.58, 0.04),
                   handlelength=2, columnspacing=1.0,
                   title="Elevation Band", title_fontproperties={"weight": "bold", "size": 10})

# Line 2: Period
period_handles = [
    Line2D([0], [0], color="#444", lw=2.5, linestyle="-", label="Historical (2000–2020)"),
    Line2D([0], [0], color="#444", lw=2.5, linestyle="--", label="Future (2080–2100)"),
]

leg2 = fig.legend(handles=period_handles, loc="lower center",
                   ncol=2, fontsize=9, frameon=False,
                   bbox_to_anchor=(0.58, 0.0),
                   handlelength=2.5, columnspacing=2.0,
                   title="Period", title_fontproperties={"weight": "bold", "size": 10})
fig.add_artist(leg1)

# --- Title ---
fig.text(0.55, 1.015, "Projected Seasonal Snow Changes Under High Emissions (SSP5-8.5)",
         ha="center", va="bottom", fontsize=14, fontweight="bold", color="#333",
         transform=fig.transFigure)

plt.savefig("policy_brief_2x2.png", dpi=250, bbox_inches="tight", facecolor="white")
plt.savefig("policy_brief_2x2.pdf", bbox_inches="tight", facecolor="white")
print("Saved: policy_brief_2x2.png and policy_brief_2x2.pdf")
