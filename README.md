```
str = \S*
uint = \d+

# drwxr-xr-- user group 12345 Sep 2 2022 Desktop 

ftype = d|l|-

perm1 = {r:"r"|"-"}{w:"w"|"-"}{x:"x"|"-"}
perm = {u:perm1}{g:perm1}{o:perm1}
perm = {u,g,o:perm1}

datetime = {month:alpha+} {day:uint} {year:uint} | {day:...

ll = {ftype}{perm} {user group:str} {size:uint} {datetime} {name:str}
```

Separators in the wild: \s+ , / ; :

### Tests

```
''                        => ""           => ""
{str}                     => "abc"        => _{str: "abc"}
alpha+                    => "abc"        => _["a", "b", "c"]
[\a+][\d+]                => "abc123"     => _["a", "b", "c", "1", "2", "3"]
{x:\a}\d*{y:\a}           => "a12b"       => _{x: "a", y: "b"}
alpha+digit+              => "abc123"     => _
{x:alpha+}{y:digit+}      => "abc123"     => _{x: "abc"["a", "b", "c"], y: "123"["1", "2", "3"]}
{x:{y:alpha}{z:alpha}}    => "ab"         => _{x: "ab"{y: "a", z: "b"}}
{x:{y:alpha}+}            => "abc"        => _{x: "abc"["a"{y: "a"}, "b"{y: "b"}, "c"{y: "c"}]}

{alpha} {alpha}           => "a  b"       => _{alpha: "ab"}
{x.y:alpha}{x.z:alpha}    => "ab"         => _{x: "ab"{y: "a", z: "b"}}
{x.y:alpha}{x.y:alpha}    => "ab"         => _{x: "ab"{y: "ab"}}
{x:{y:\a}{z:\a}}{x.y:\a}  => "abc"        => _{x: "abc"{y: "ac", z: "b"}}
{x:alpha+} {x:alpha+}     => "a b"        => _{x: "ab"["a", "b"]}
[alpha+] [alpha+]         => "a b"        => _["a", "b"]
[alpha+] alpha+           => "a b"        => _["a"]

{x:alpha|digit}           => "a"          => _{x: "a"}
{x:[alpha+][digit]}       => "ab1"        => _{x: "ab1"}
{x:[alpha+]\,[digit]}     => "ab,1"       => _{x: "ab,1"}
{x:[digit+]|{y:alpha}}    => "123"        => _["1", "2", "3"]
{x:[digit+]|{y:alpha}}    => "a"          => _{y: "a"}
{str:{str:str}}           => "abc"        => _{str: "abc"{str: "abc"}}

abc                       => 'abcd'       => abcd
                                                ^ expected EOF, found 'd'

abc                       => 'ab'         => ab
                                               ^ expected 'c', found EOF

{x:[digit+]{alpha}}       =>  Types do not match:
                                {x:[digit+]{alpha}}
                                 ^^^^^^^^^^ implies that x is an array
                                {x:[digit+]{alpha}}
                                 ^^        ^^^^^^^ implies that x is a record (`{alpha}` is synonym for `{alpha:alpha}`)

{x.y:alpha+}{x.y.z:digit} =>  Types do not match:
                                {x.y:alpha+}{x.y.z:digit}
                                 ^^^^^^^^^^ implies that x.y is an array
                                {x.y:alpha+}{x.y.z:digit}
                                             ^^^^^^^^^^^ implies that x.y is a record
```
