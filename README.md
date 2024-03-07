# loam-chart

  

Quick thing I wanted to throw together. Takes in clay and sand percentage and determines soil type based on the soil texture triangle.

  

May end up using this in some other project but for now it's just an application of the crossing number raycast algorithm in Fortran

## Contents
**Polygons Module**
- Create polygon by providing name and array of point vertices
`Points defined as two real variables representing x and y`

**ZoneCalc Module**
- Assign all twelve zones with associated vertices
- Line intercept function
- Get potential zones based on closest vertices to intercept point
- Cast function which imitates a straight line and checks if it intercepts the edges of closest polygon(s) 
- Function to deallocate points