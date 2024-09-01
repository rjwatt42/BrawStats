getTypecombination<-function(HypType) {

  IV<-getVariable("IV")
  IV2<-getVariable("IV2")
  DV<-getVariable("DV")

  switch (HypType,
    "ee"={
            IV2<-NULL
    },
    "i~i"={ #print(1)
      IV$type<-"Interval"
      DV$type<-"Interval"
      IV2<-NULL
    },
    "i~o"={ #print(1)
      IV$type<-"Ordinal"
      DV$type<-"Interval"
      IV2<-NULL
    },
    "i~c2"={ #print(2)
      IV$type<-"Categorical"
      IV$ncats<-2
      IV$cases<-"C1,C2"
      IV$proportions<-"1,1"
      DV$type<-"Interval"
      IV2<-NULL
    },
    "i~c3"={ #print(2)
      IV$type<-"Categorical"
      IV$ncats<-3
      IV$cases<-"C1,C2,C3"
      IV$proportions<-"1,1,1"
      DV$type<-"Interval"
      IV2<-NULL
    },
    "o~i"={ #print(3)
      IV$type<-"Interval"
      DV$type<-"Ordinal"
      IV2<-NULL
    },
    "o~o"={ #print(3)
      IV$type<-"Ordinal"
      DV$type<-"Ordinal"
      IV2<-NULL
    },
    "o~c2"={ #print(4)
      IV$type<-"Categorical"
      IV$ncats<-2
      IV$cases<-"C1,C2"
      IV$proportions<-"1,1"
      DV$type<-"Ordinal"
      IV2<-NULL
    },
    "o~c3"={ #print(4)
      IV$type<-"Categorical"
      IV$ncats<-3
      IV$cases<-"C1,C2,C3"
      IV$proportions<-"1,1,1"
      DV$type<-"Ordinal"
      IV2<-NULL
    },
    "c~i"={ #print(5)
      IV$type<-"Interval"
      DV$type<-"Categorical"
      IV2<-NULL
    },
    "c~o"={ #print(5)
      IV$type<-"Ordinal"
      DV$type<-"Categorical"
      IV2<-NULL
    },
    "c~c2"={ #print(6)
      IV$type<-"Categorical"
      IV$ncats<-2
      IV$cases<-"C1,C2"
      IV$proportions<-"1,1"
      DV$type<-"Categorical"
      IV2<-NULL
    },
    "c~c3"={ #print(6)
      IV$type<-"Categorical"
      IV$ncats<-3
      IV$cases<-"C1,C2,C3"
      IV$proportions<-"1,1,1"
      DV$type<-"Categorical"
      IV2<-NULL
    },
    
       "i~i+i"={ #print(11)
              IV$type<-"Interval"
              IV2$type<-"Interval"
              DV$type<-"Interval"
            },
       "i~c+i"={ #print(12)
              IV$type<-"Categorical"
              IV$ncats<-3
              IV$cases<-"C1,C2,C3"
              IV$proportions<-"1,1,1"
              IV2$type<-"Interval"
              DV$type<-"Interval"
            },
       "i~i+c"={ #print(13)
              IV$type<-"Interval"
              IV2$type<-"Categorical"
              IV2$ncats<-3
              IV2$cases<-"D1,D2,D3"
              IV2$proportions<-"1,1,1"
              DV$type<-"Interval"
            },
       "i~c+c"={ #print(14)
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
       "c~i+i"={ #print(15)
              IV$type<-"Interval"
              IV2$type<-"Interval"
              DV$type<-"Categorical"
              DV$ncats<-2
              DV$cases<-"E1,E2"
              DV$proportions<-"1,1"
            },
    
    "i~w+i"={ #print(12)
      IV$type<-"Categorical"
      IV$ncats<-3
      IV$cases<-"C1,C2,C3"
      IV$proportions<-"1,1,1"
      IV$deploy<-"Within"
      IV2$type<-"Interval"
      DV$type<-"Interval"
    },
    "i~i+w"={ #print(13)
      IV$type<-"Interval"
      IV2$type<-"Categorical"
      IV2$ncats<-3
      IV2$cases<-"D1,D2,D3"
      IV2$proportions<-"1,1,1"
      IV2$deploy<-"Within"
      DV$type<-"Interval"
    },
    "i~w+c"={ #print(14)
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
    "i~w+w"={ #print(14)
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
    
    "c~i+i"={ #print(15)
      IV$type<-"Interval"
      IV2$type<-"Interval"
      DV$type<-"Categorical"
      DV$ncats<-2
      DV$cases<-"E1,E2"
      DV$proportions<-"1,1"
    },
    
       "c~c+i"={ #print(16)
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
       "c~i+c"={ #print(17)
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
       "c~c+c"={ #print(18)
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

