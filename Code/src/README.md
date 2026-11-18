# Zoogeochemical engineers: Galápagos giant tortoises link terrestrial and freshwater ecosystems

![Galapagos Tortoises](https://www.mpg.de/11588404/original-1626697505.webp?t=eyJ3aWR0aCI6NjgyLCJmaWxlX2V4dGVuc2lvbiI6IndlYnAiLCJvYmpfaWQiOjExNTg4NDA0fQ%3D%3D--93af1a5a49a0a5744d3d0bd62b2d96c67767ef27)

Analysis code for Ellis-Soto et al., *"Zoogeochemical engineers: Galápagos
giant tortoises link terrestrial and freshwater ecosystems through sediment
transport and nutrient dynamics"*.

Diego Ellis-Soto — diego.ellissoto@berkeley.edu

---

## What is here

Every figure, table and number in the manuscript is produced by **four
scripts in `Code/src/`**. Run them from the project root. The first three are independent; the fourth
formats results written by the third, so run `pond_chemistry_pca.R` before
`pond_table_supplementary.R`. Each ends by printing the values it contributes
to the manuscript, so a reader can check the text against the output without
opening a figure.

```
Rscript Code/src/pond_revisitation.R
Rscript Code/src/pond_mud_camera.R
Rscript Code/src/pond_chemistry_pca.R
Rscript Code/src/pond_table_supplementary.R
```

R 4.4.1. Packages: tidyverse, janitor, lubridate, sf, terra, move, keyring,
recurse, vegan, glmmTMB, ggeffects, DHARMa, emmeans, patchwork, ggrepel,
viridis, scales, leaflet, mapview, htmlwidgets, bipartite.

---

## The four scripts

### 1. `pond_revisitation.R` — telemetry, pond use, sediment export

Builds the pond outlines, finds which tracked tortoises entered them, and
reconstructs every visit with the `recurse` package.

**Produces:** Figure 1 (study-area map), Figure 2 (visit duration by entrance
time; pond residence by hour of day), Figure 3 (tortoise–pond network),
Supplementary Figure 1 (overnight stays), Supplementary Figure 4 (sediment
export).
**Key numbers:** 2,254 visits by 21 tortoises at 12 ponds; median residence
0.90 h; 182 overnight visits (8.07%); 47 pond transitions; 829 departure days
over 51.2 tortoise-years; 8.20 kg of sediment exported per tortoise per year.
**Inputs:** Movebank study 2928116 (cached in `Data/tortoises_movebank_*.Rdata`),
`Data/pond_2025_post_fieldwork_clean.csv`, `Data/Mud_2024_2025_fieldwork.csv`,
`Data/SRTM_Santa_Cruz.tif`.
**Output:** `Outdir/recurse_polygon/`

*How a pond is defined:* ponds shrink and grow between years, so a pond is
everything within 5 m of any GPS position recorded at its margin during water
sampling, pooled across all years. Three ponds too large for that treatment —
Cerro Mesa, Laguna Chato and Manzanillo 1 — were digitised from imagery and
buffered by the same 5 m. Visits of ≤0.1 h are dropped as pass-bys, and
records sharing tortoise, pond, entrance and exit time are collapsed so a pond
represented by several sampling positions does not count one visit twice.

### 2. `pond_mud_camera.R` — sediment loads and the Montemar camera

**Produces:** Figure 4 (camera: tortoises in water by hour and by month),
Figure 5 (sediment by pond type), Supplementary Figure 3 (sediment vs carapace
length), and the pond size-class lookup.
**Key numbers:** 42 tortoises sampled for sediment, 26 with carapace length;
median 340 g, µ 505.9, SD 518.9, range 0–2,112 g; touristic 2.02× (95% CI
1.00–4.10, P = 0.050); 18,235 camera images, 7 Apr 2022 – 11 Jan 2023.
**Inputs:** `Data/Mud_2024_2025_fieldwork.csv`,
`Data/Photo_spreadsheet_montemar_all.csv`,
`Data/pond_2025_post_fieldwork_clean.csv`.
**Output:** `Outdir/pond_mud_camera/`

*The sediment correction:* sediment was scraped from the anterior carapace
only in 2018–2020 and from both the anterior and posterior portions from 2021,
so pre-2021 loads are doubled to put every year on the same whole-carapace
basis. This is derived in code from the raw `Mud weight (g)` column and the
year. The stored `Mud weight adressing half mud in 2019(g)` column is **not**
used: it doubles 2019 only, and one 2020 record holds a value filled down from
the row above.

### 3. `pond_chemistry_pca.R` — ordination and nutrient models

**Produces:** Figure 6 (PCA plus the three nutrient relationships),
Supplementary Figure 2 (within- and between-pond partitioning), the full
response-ratio table, the TSS models, the ammonium leave-one-pond-out check
and the DHARMa residual tests.
**Key numbers:** 67 sampling events at 21 ponds; PC1 34.7%, PC2 20.3%;
ammonium RR 1.45 (1.16–1.80, P = 0.001), NPOC 1.11 (P = 0.011), TP 1.19
(P = 0.022); TSS 3.42× higher at touristic ponds (P = 0.009); ammonium robust
to dropping any single pond (RR 1.33–1.52, largest P = 0.013).
**Inputs:** `Data/pond_2025_post_fieldwork_clean_2026_deep_dive.csv`,
`Data/chlorophyl_data_clean_deep_clean.csv`,
`Outdir/final/TSS_pond_identity/TSS_RAW_with_clean_pond_identity.csv`.
**Output:** `Outdir/pond_chemistry_pca/`

*Active and supplementary variables:* the ordination is built from eight
measured water variables. Tortoise abundance, TSS, chlorophyll-a and elevation
are fitted **after** the ordination and never help to define it — making TSS
active would cut the sample from 67 events to 46 and let TSS shape the axis it
is then shown to correlate with. Because these four are fitted independently,
each uses all the events where it was measured, so incomplete coverage does
not shrink the ordination.

### 4. `pond_table_supplementary.R` — Supplementary Tables 1 and 2

**Table 1** is one row per pond: site, type, coordinates, elevation, sampling
effort, and every physicochemical and nutrient variable as mean ± SD (n).
**Table 2** collects every association between tortoise abundance and pond
chemistry: the event-level response ratio and the between- and within-pond
components for all eight responses.
**Key numbers:** 21 ponds, 157 sampling events, 138–424 m elevation.
**Output:** `Outdir/pond_table_supplementary/`, each table as CSV and as HTML
(open the HTML, select the table, copy — Word and Google Docs keep the cells).

*This is the one script with an order dependency.* Table 2 formats the model
results written by `pond_chemistry_pca.R`, so run that first; the models are
fitted in one place only, so the two scripts cannot disagree. The script stops
with a clear message if those results are missing.

*Elevation is SRTM*, extracted from the digital elevation model at each pond's
coordinates, which is the 138–424 m range quoted in the Study Site. The
handheld GPS readings (149–424 m) stay in the data and are what the PCA uses
as a supplementary variable, but only one elevation is reported in the table
so there is nothing for a reader to reconcile.

---

## Where each manuscript figure comes from

| Manuscript item | Script |
|---|---|
| Figure 1 — study area and migration routes | `pond_revisitation.R` |
| Figure 2 — revisitation patterns | `pond_revisitation.R` |
| Figure 3 — tortoise–pond network | `pond_revisitation.R` |
| Figure 4 — Montemar camera | `pond_mud_camera.R` |
| Figure 5 — sediment by pond type | `pond_mud_camera.R` |
| Figure 6 — PCA and nutrient models | `pond_chemistry_pca.R` |
| Suppl. Figure 1 — overnight pond use | `pond_revisitation.R` |
| Suppl. Figure 2 — within/between ponds | `pond_chemistry_pca.R` |
| Suppl. Figure 3 — sediment vs carapace | `pond_mud_camera.R` |
| Suppl. Figure 4 — sediment export | `pond_revisitation.R` |
| Suppl. Table 1 — pond characteristics | `pond_table_supplementary.R` |
| Suppl. Table 2 — response ratios | `pond_table_supplementary.R` |

---

## Things that will bite you

- **Movebank snapshot.** `pond_revisitation.R` reads a cached `.Rdata`
  snapshot. Leaving `movebank_snapshot <- NULL` takes the newest file in
  `Data/`, so tracking-day totals drift as snapshots accumulate. Pin it to one
  file to reproduce published numbers exactly.
- **Timezones.** Movebank timestamps are UTC and Galápagos is UTC−6. `as.Date()`
  defaults to UTC and `read_csv` re-tags POSIXct as UTC, either of which
  silently shifts every diel result by six hours. Pass `tz` explicitly.
- **Pond type colours.** Non-touristic is blue, touristic is gold, in every
  figure.
- **One excluded record.** P_Laguna_Ch, 21-Jun-18 10:21 is dropped everywhere:
  its conductivity is 1,918 µS/cm against 61–157 at every other visit to that
  pond, and a record at identical coordinates was already removed by the field
  team as a different pond. Dropping it changes no conclusion.
- **Permutation P-values.** The supplementary-variable fits in the PCA use
  4,999 permutations, so their P-values move in the third decimal between runs
  even with the seed set.

---

## Everything else in `Code/`

The numbered scripts (`00_`–`35_`) and the unnumbered ones are the exploratory
path that led to the four above, as are the scripts in `Code/src/old/`. They
are kept for provenance only. Several hardcode numbers that are now stale, so
do not run them expecting current results.

Data are available from the corresponding author; tortoise telemetry is
archived on Movebank (study 2928116).
