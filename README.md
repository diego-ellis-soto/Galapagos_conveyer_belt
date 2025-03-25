# Giant tortoise ecosystem services connecting terrestrial and aquatic food webs 


![Galapagos Tortoises](https://www.mpg.de/11588404/original-1626697505.webp?t=eyJ3aWR0aCI6NjgyLCJmaWxlX2V4dGVuc2lvbiI6IndlYnAiLCJvYmpfaWQiOjExNTg4NDA0fQ%3D%3D--93af1a5a49a0a5744d3d0bd62b2d96c67767ef27)

Diego Ellis Soto, diego.ellissoto@berkeley.edu

# Order of scripts necessary to replicate analysis:

find_duplicated_ponds.R: Clean pond locations (fix missing - signs in Long/Lat), make a map

Make_table_ponds.R: Create plot and table of sampling across ponds through time

To do: Add pond size: Small-Medium-Large categorical

Next steps: Re-run recurse analysis with buffers depending on pond size category

Freddy-Christian:  iButton at Montemar, 

### Code book:

find_duplicated_ponds.R: Create a clean file of pond locations.

Make_table_ponds.R: Make a table of sampling regimes of ponds.

Get_precip.R

buffer_ponds.R: Create a buffer around pond locations and create shapefiles for larger ponds

pond_camera_mud.R: Analyze mud transport, trap camera tortoise abundances, PCA of physico-chemical and nutrient analysis

make_map_study.R: Makes a map of tracks, ponds and elevation contours