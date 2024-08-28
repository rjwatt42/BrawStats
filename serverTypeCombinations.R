getTypecombination<-function(HypType) {

  IV<-getVariable("IV")
  IV2<-getVariable("IV2")
  DV<-getVariable("DV")
  switch (HypType,
    "ee"={
            IV2<-NULL
    },
    "ii"={ #print(1)
      IV$type<-"Interval"
      DV$type<-"Interval"
      IV2<-NULL
    },
    "oi"={ #print(1)
      IV$type<-"Ordinal"
      DV$type<-"Interval"
      IV2<-NULL
    },
    "c2i"={ #print(2)
      IV$type<-"Categorical"
      IV$ncats<-2
      IV$cases<-"C1,C2"
      IV$proportions<-"1,1"
      DV$type<-"Interval"
      IV2<-NULL
    },
    "c3i"={ #print(2)
      IV$type<-"Categorical"
      IV$ncats<-3
      IV$cases<-"C1,C2,C3"
      IV$proportions<-"1,1,1"
      DV$type<-"Interval"
      IV2<-NULL
    },
    "io"={ #print(3)
      IV$type<-"Interval"
      DV$type<-"Ordinal"
      IV2<-NULL
    },
    "oo"={ #print(3)
      IV$type<-"Ordinal"
      DV$type<-"Ordinal"
      IV2<-NULL
    },
    "c2o"={ #print(4)
      IV$type<-"Categorical"
      IV$ncats<-2
      IV$cases<-"C1,C2"
      IV$proportions<-"1,1"
      DV$type<-"Ordinal"
      IV2<-NULL
    },
    "c3o"={ #print(4)
      IV$type<-"Categorical"
      IV$ncats<-3
      IV$cases<-"C1,C2,C3"
      IV$proportions<-"1,1,1"
      DV$type<-"Ordinal"
      IV2<-NULL
    },
    "ic"={ #print(5)
      IV$type<-"Interval"
      DV$type<-"Categorical"
      IV2<-NULL
    },
    "oc"={ #print(5)
      IV$type<-"Ordinal"
      DV$type<-"Categorical"
      IV2<-NULL
    },
    "c2c"={ #print(6)
      IV$type<-"Categorical"
      IV$ncats<-2
      IV$cases<-"C1,C2"
      IV$proportions<-"1,1"
      DV$type<-"Categorical"
      IV2<-NULL
    },
    "c3c"={ #print(6)
      IV$type<-"Categorical"
      IV$ncats<-3
      IV$cases<-"C1,C2,C3"
      IV$proportions<-"1,1,1"
      DV$type<-"Categorical"
      IV2<-NULL
    },
    
       "iii"={ #print(11)
              IV$type<-"Interval"
              IV2$type<-"Interval"
              DV$type<-"Interval"
            },
       "cii"={ #print(12)
              IV$type<-"Categorical"
              IV$ncats<-3
              IV$cases<-"C1,C2,C3"
              IV$proportions<-"1,1,1"
              IV2$type<-"Interval"
              DV$type<-"Interval"
            },
       "ici"={ #print(13)
              IV$type<-"Interval"
              IV2$type<-"Categorical"
              IV2$ncats<-3
              IV2$cases<-"D1,D2,D3"
              IV2$proportions<-"1,1,1"
              DV$type<-"Interval"
            },
       "cci"={ #print(14)
              IV$type<-"Categorical"
              IV$ncats<-3
              IV$cases<-"C1,C2,C3"
              IV$proportions<-"1,1,1"
              IV2$type<-"Categorical"
              IV2$ncats<-3
              IV2$cases<-"D1,D2,D3"
              IV2$proportions<-"1,1,1"
              DV$type<-"Interval"
            },
       "iic"={ #print(15)
              IV$type<-"Interval"
              IV2$type<-"Interval"
              DV$type<-"Categorical"
              DV$ncats<-2
              DV$cases<-"E1,E2"
              DV$proportions<-"1,1"
            },
    
    "wii"={ #print(12)
      IV$type<-"Categorical"
      IV$ncats<-3
      IV$cases<-"C1,C2,C3"
      IV$proportions<-"1,1,1"
      IV$deploy<-"Within"
      IV2$type<-"Interval"
      DV$type<-"Interval"
    },
    "iwi"={ #print(13)
      IV$type<-"Interval"
      IV2$type<-"Categorical"
      IV2$ncats<-3
      IV2$cases<-"D1,D2,D3"
      IV2$proportions<-"1,1,1"
      IV2$deploy<-"Within"
      DV$type<-"Interval"
    },
    "wci"={ #print(14)
      IV$type<-"Categorical"
      IV$ncats<-3
      IV$cases<-"C1,C2,C3"
      IV$proportions<-"1,1,1"
      IV$deploy<-"Within"
      IV2$type<-"Categorical"
      IV2$ncats<-3
      IV2$cases<-"D1,D2,D3"
      IV2$proportions<-"1,1,1"
      DV$type<-"Interval"
    },
    "wwi"={ #print(14)
      IV$type<-"Categorical"
      IV$ncats<-3
      IV$cases<-"C1,C2,C3"
      IV$proportions<-"1,1,1"
      IV$deploy<-"Within"
      IV2$type<-"Categorical"
      IV2$ncats<-3
      IV2$cases<-"D1,D2,D3"
      IV2$proportions<-"1,1,1"
      IV2$deploy<-"Within"
      DV$type<-"Interval"
    },
    
    "iic"={ #print(15)
      IV$type<-"Interval"
      IV2$type<-"Interval"
      DV$type<-"Categorical"
      DV$ncats<-2
      DV$cases<-"E1,E2"
      DV$proportions<-"1,1"
    },
    
       "cic"={ #print(16)
              IV$type<-"Categorical"
              IV$ncats<-3
              IV$cases<-"C1,C2,C3"
              IV$proportions<-"1,1,1"
              IV2$type<-"Interval"
              DV$type<-"Categorical"
              DV$ncats<-2
              DV$cases<-"E1,E2"
              DV$proportions<-"1,1"
            },
       "icc"={ #print(17)
              IV$type<-"Interval"
              IV2$type<-"Categorical"
              IV2$ncats<-3
              IV2$cases<-"C1,C2,C3"
              IV2$proportions<-"1,1,1"
              DV$type<-"Categorical"
              DV$ncats<-2
              DV$cases<-"E1,E2"
              DV$proportions<-"1,1"
            },
       "ccc"={ #print(18)
              IV$type<-"Categorical"
              IV$ncats<-3
              IV$cases<-"C1,C2,C3"
              IV$proportions<-"1,1,1"
              IV2$type<-"Categorical"
              IV2$ncats<-3
              IV2$cases<-"C1,C2,C3"
              IV2$proportions<-"1,1,1"
              DV$type<-"Categorical"
              DV$ncats<-2
              DV$cases<-"E1,E2"
              DV$proportions<-"1,1"
            },
    )
    result<-list(IV=IV, IV2=IV2, DV=DV)
}

