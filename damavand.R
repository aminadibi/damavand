library(raster)
library(sp)
library(rayshader)
library(elevatr)

#Area of Interest
AOI <- function(p, q, r, s) {   
  xleft <- p   
  ytop <- q   
  xright <- r   
  ybot <- s   
  x_coord <- c(xleft,xleft,xright,xright)   
  y_coord <- c(ytop,ybot, ybot, ytop)   
  xym <- cbind(x_coord, y_coord)   
  p <- Polygon(xym)   
  ps = Polygons(list(p),1)   
  sps = SpatialPolygons(list(ps))   
  proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")   
  return(sps) } 

#mask <- AOI(51.7, 36.5, 52.3, 35.4)  
mask <- AOI(49.8, 49.99, -123.07, -122.99)  

Elevation_File <- get_elev_raster(mask, z=11) 

projectRaster(Elevation_File,               
              crs= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 


localtif_mask <- crop(Elevation_File, mask) 


elmat2 <- matrix(raster::extract(localtif_mask,
                                 raster::extent(localtif_mask),
                                 buffer=1000),nrow=ncol(localtif_mask),
                 ncol=nrow(localtif_mask)) 

raymat2 <- ray_shade(elmat2) 
ambmat2 <- ambient_shade(elmat2) 

elmat2 %>%
  sphere_shade(sunangle = 225 ,texture = "imhof1") %>%
  add_water(detect_water(elmat2, min_area = 100), color="imhof1") %>%
  add_shadow(ray_shade(elmat2,zscale=3,maxsearch = 300),0.5) %>%
  add_shadow(ambmat2,0.5)%>%
  plot_3d(elmat2, zscale = 20, theta = 0, zoom = 0.3, 
          phi = 25, water = TRUE, windowsize = c(1366,768))
