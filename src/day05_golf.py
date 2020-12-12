r=str.replace;print(max(map(lambda x:int(r(r(r(r(x,'F','0'),'B','1'),'R','1'),'L','0'),2),open("../content/day05"))))
s=set(map(lambda x:int(r(r(r(r(x,'F','0'),'B','1'),'R','1'),'L','0'),2),open("../content/day05")));print([i for i in s if (i+1) not in s and (i+2) in s][0])
