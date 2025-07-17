module SawToothBugCrawl exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.Secret as GSVG



plot f t shape  intScale = 
  let
    --loops every 10/intScale secs ie intScale = , only show 5 seconds
    timeSpan = 10 / intScale 
    
    --do this bc no mod in elm 
    --floor (t/timeSpan) gives number in range 0 to how many cycles have happened
    --mult cycle number by TimeSpan to subtract the correct number
    -- take time minus that number
    -- ie if t = 21 sec passed
    --floor of 21/5 = 4, so on our fourth cycle
    -- mult that by 5, time Span to get 20 so we do 21-20 = 1
    --so on our 1/5 time interval 
    -- timeinteravl always returns numbner 0-4
    timeInInterval = t - (timeSpan * toFloat (floor (t / timeSpan)))

    --use constantn num of pts but map over a shorter range
    --toFloat x / toFloat 192 gives a value from 0.0 to 1.0
    --multiply by timeSpan to strech to the correct time frame ie 0-10 or 0-5
    allTimes = List.range 0 192
                 |> List.map (\x -> (toFloat x / toFloat 192) * timeSpan)

    --adjust the sizE ON SCREEN
    --awlays want to occupy 192 pixels wide on screen 
    --if timeSpan = 10 then each time unit is 19.2 pixels, if 5 then each is 38.4 pixels
    xScale = 192 / timeSpan
    yScale = xScale  -- have this so doesnt look vertically squished or anything 
    
    
    --convers a time into a screen cooridnate (x,y)
    --  -96 to +96 is 192px
    --  -96 centers the graph horizontally 
    --    then add the xScale * the time 
    -- yscale * (ftt) scaled the function output to screen space 
    plotOne tt = (-96 + xScale * tt, yScale * (f tt))
  in
  
  --openPolygon (List.map plotOne allTimes) converts each time in allTimes to a (x,y) cooridneate pt that is drawn
  -- shape |> move (plotOne timeInInterval) animates the shape, moving it over time since timeInInteravel loops smoothly from 0 to timeSpan
    [ openPolygon (List.map plotOne allTimes) |> outlined (solid 0.5) black
    , shape |> move (plotOne timeInInterval)
    ]
      |> group


sawtooth t = 
  let
    tInt = floor t
    tFrac = 0.2*t - 0.2*(toFloat tInt)
  in 
    tFrac 
    




branchAnimation model =  group [
     branch
  |> scaleY 1.1
  |> scaleX 2
  |> move (-85, 10)
  ,
  ladyBug
  |> scale 0.2
  |> rotate (degrees -67)
  |> move (0, -40)
  |> move (1, 82*(sawtooth (0.5* model.time)))
  ]



branch = group [ curve (38.160,-51.55) [GSVG.Pull (38.698,-38.87) (39.596,-26.19),GSVG.Pull (34.830,-15.79) (30.504,-4.426),GSVG.Pull (32.896,-0.760) (35.528,-8.014),GSVG.Pull (37.682,-13.03) (39.835,-18.06),GSVG.Pull (39.595,7.6727) (40.074,35.528),GSVG.Pull (40.251,38.358) (41.988,42.228),GSVG.Pull (44.146,40.115) (43.663,37.203),GSVG.Pull (41.982,34.495) (44.142,16.867),GSVG.Pull (46.611,23.629) (50.841,28.351),GSVG.Pull (51.064,25.682) (48.448,20.934),GSVG.Pull (46.414,14.412) (44.381,9.6897),GSVG.Pull (44.863,-21.29) (42.945,-51.55),GSVG.Pull (40.433,-51.67) (37.921,-51.79)]
  |> filled (rgb 115 74 9) ]

ladyBug = group [
      --body
  curve (-26.67,14.714) [GSVG.Pull (-14.80,22.864) (2.9906,20.934),GSVG.Pull (24.224,16.070) (32.657,-1.794),GSVG.Pull (39.334,-29.23) (22.130,-40.55),GSVG.Pull (6.2065,-53.68) (-13.51,-47.01),GSVG.Pull (-33.09,-40.79) (-40.55,-21.65),GSVG.Pull (-34.53,-2.828) (-26.67,14.474)]
  |> filled red
  ,
  circle 4
  |> filled black
  |> move (5, -1)
  ,
  circle 4.8
  |> filled black
  |> move (-2, 13.5)
  ,
  circle 5.7
  |> filled black
  |> move (-20, 6)
  ,
  circle 5.6
  |> filled black
  |> move (24, -11.5)
  ,
  circle 5
  |> filled black
  |> move (13, -35.5)
  ,
  circle 5.8
  |> filled black
  |> move (-7.5, -23.5)
  ,
  circle 3.5
  |> filled black
  |> move (-30.5, -14.5)
  ,
  circle 4
  |> filled black
  |> move (-3.5, -41.5)
  ,
  curve (19.738,13.517) [GSVG.Pull (19.568,7.7259) (25.719,8.2542)]
  |> filled black
  |> scale 1.3   |> move (-5.2,-4)
  ,
  curve (13.996,-19.97) [GSVG.Pull (22.848,-22.96) (31.700,-25.95),GSVG.Pull (31.204,-29.14) (29.547,-31.70),GSVG.Pull (21.652,-25.95) (13.757,-20.21)]
  |> filled white
  |> move (1.3,-1.3)
  ,
  curve (-34.09,-32.65) [GSVG.Pull (-31.92,-30.57) (-28.35,-32.65),GSVG.Pull (-26.28,-35.72) (-30.02,-37.92)]
  |> filled black
  |> move (-0.3,-0.3)
  -- head
 ,
  curve (-26.67,15.910) [GSVG.Pull (-43.16,25.495) (-56.82,12.8),GSVG.Pull (-64.75,-0.647) (-59.69,-8.254),GSVG.Pull (-54.89,-16.73) (-46.77,-20.21),GSVG.Pull (-43.66,-21.71) (-40.55,-21.17)]
  |> filled (rgb 111 116 125)
  ,
  
  curve (-43.42,-20.21) [GSVG.Pull (-41.98,-20.81) (-40.55,-21.41),GSVG.Pull (-33.25,-3.110) (-25.95,15.192),GSVG.Pull (-26.99,16.309) (-28.59,16.867)]
  |> filled (rgb 111 116 125)
  ,
  circle 3
  |> filled black
  |> move (-45, 10)
  ,
  circle 1
  |> filled white
  |> move (-44, 8)
  ,
  circle 3
  |> filled black
  |> move (-51, -7)
  ,
  circle 1
  |> filled white
  |> move (-50, -9)
  ,
  circle 2
  |> filled (rgb 237 171 213)
  |> move (-57, -11)
  ,
  circle 2
  |> filled (rgb 237 171 213)
  |> move (-49, 16.5)
  ,
  --mouth
  
  curve (-54.66,8.2542) [GSVG.Pull (-59.54,5.9442) (-56.10,1.7943)]
  |> outlined (solid 1) black
  ,
  --antenna
  
  curve (-28.35,17.345) [GSVG.Pull (-18.32,21.245) (-20.93,30.265),GSVG.Pull (-24.54,33.005) (-27.39,30.265)]
  |> outlined (solid 1) black
  |> move (0.5, -0.5)
  ,
  circle 1.8
  |> filled red
  |> move (-27, 29)
  , 
  curve (-41.51,-20.93) [GSVG.Pull (-38.62,-33.91) (-44.38,-34.81),GSVG.Pull (-48.79,-35.21) (-49.16,-29.78)]
  |> outlined (solid 1) black
  ,
  circle 1.8
  |> filled red
  |> move (-49, -30)
  ,
  -- legs
  curve (-33.13,-33.13) [GSVG.Pull (-35.46,-38.74) (-41.98,-38.63),GSVG.Pull (-45.89,-41.59) (-41.03,-42.94),GSVG.Pull (-35.18,-41.43) (-31.22,-35.76)]
  |> filled (rgb 28 23 26)
  |> move (-0.3,-1)
  ,
  
  curve (-13.27,-46.53) [GSVG.Pull (-13.41,-53.34) (-18.06,-55.62),GSVG.Pull (-17.69,-60.64) (-12.08,-57.30),GSVG.Pull (-10.00,-53.39) (-9.450,-47.73)]
  |> filled (rgb 28 23 26)
  ,
  
  curve (13.517,-45.57) [GSVG.Pull (17.015,-52.82) (14.714,-55.62),GSVG.Pull (19.485,-60.54) (19.977,-53.71),GSVG.Pull (19.183,-48.28) (16.388,-44.38)]
  |> filled (rgb 28 23 26)
  ,
  
  curve (33.375,-3.469) [GSVG.Pull (40.499,-3.120) (43.663,3.7084),GSVG.Pull (42.213,9.4491) (37.442,3.2299),GSVG.Pull (35.970,-0.243) (32.897,-1.076)]
  |> filled (rgb 28 23 26)  
  ,
  
  curve (14.953,16.867) [GSVG.Pull (19.207,18.470) (18.542,27.394),GSVG.Pull (14.448,30.620) (14.235,23.566),GSVG.Pull (15.277,19.954) (13.039,18.302)]
  |> filled (rgb 28 23 26) 
  |> move (-0.7,-0.5)
  ,
  curve (-9.689,21.652) [GSVG.Pull (-9.724,28.235) (-13.27,32.179),GSVG.Pull (-21.03,34.325) (-16.38,28.351),GSVG.Pull (-11.87,25.741) (-12.56,21.652)]
  |> filled (rgb 28 23 26) 
  |> move (0.6, -0.7)
    
     ]