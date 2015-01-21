#include <math.h>
#include <stdio.h>

typedef float f32;

struct charge {
  f32 val, x, y, z;
};

void write_charge(struct charge *chg, int i, f32 val, f32 x, f32 y, f32 z) {
  chg[i] = (struct charge) {
    val = val,
    x = x,
    y = y,
    z = z
  };
}

/*
chg - array of (val, x, y, z) representing electric charges
n - number of charges

buf - output buffer

x, y, z - starting point

s - number of points to simulate

ma, mb - max near and far distances
*/
int work(struct charge *chg, int n, f32 *buf, f32 x, f32 y, f32 z, int s, f32 ma, f32 mb) {
  int k = 0;
  
  ma *= ma;
  mb *= mb;
  
  #define WRITE() do{ buf[k*3]=x; buf[k*3+1]=y; buf[k*3+2]=z; ++k; }while(0)
  
  WRITE();
  
  for (int i = 0; i < s; ++i) {
    f32 ex, ey, ez;
    
    ex = ey = ez = 0.0f;
    
    for (int j = 0; j < n; ++j) {
      f32 dx, dy, dz;
      
      dx = x - chg[j].x;
      dy = y - chg[j].y;
      dz = z - chg[j].z;
      
      f32 d = dx*dx + dy*dy + dz*dz;
      
      //printf("%f\n", d);
      
      f32 fact = chg[j].val / (d * sqrt(d));
      
      //printf("%f\n", fact);
      
      dx *= fact;
      dy *= fact;
      dz *= fact;
      
      ex += dx;
      ey += dy;
      ez += dz;
    }
    
    //printf("%f %f %f\n", ex, ey, ez);
    
    f32 fact = 1 / sqrt(ex*ex + ey*ey + ez*ez);
    
    ex *= fact;
    ey *= fact;
    ez *= fact;
    
    x += ex;
    y += ey;
    z += ez;
    
    WRITE();
    
    if (i % 10 == 0) {
      f32 dist = x*x + y*y + z*z;
      
      if (dist > mb) {
        break;
      }
      
      for (int j = 0; j < n; ++j) {
        f32 dx, dy, dz;
        
        dx = x - chg[j].x;
        dy = y - chg[j].y;
        dz = z - chg[j].z;
        
        f32 d = dx*dx + dy*dy + dz*dz;
        
        if (d < ma) {
          goto complete;
        }
      }
    }
  }
  
complete:
  return k;
  
  #undef WRITE
}
