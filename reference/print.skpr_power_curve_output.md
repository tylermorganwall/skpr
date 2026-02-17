# Print evaluation information

Prints design evaluation information below the data.frame of power
values

Note: If options("skpr.ANSI") is `NULL` or `TRUE`, ANSI codes will be
used during printing to prettify the output. If this is `FALSE`, only
ASCII will be used.

## Usage

``` r
# S3 method for class 'skpr_power_curve_output'
print(x, ...)
```

## Arguments

- x:

  The x of the evaluation functions in skpr

- ...:

  Additional arguments.

## Examples

``` r
#Generate/evaluate a design and print its information
factorialcoffee = expand.grid(cost = c(1, 2),
                              type = as.factor(c("Kona",
                                                 "Colombian",
                                                 "Ethiopian",
                                                 "Sumatra")),
                              size = as.factor(c("Short",
                                                 "Grande",
                                                 "Venti")))

coffee_curves = calculate_power_curves(candidateset = factorialcoffee,
                                      model = ~(cost + size + type)^2,
                                      trials = 30:40, plot_results = FALSE)
coffee_curves
#>       parameter            type     power trials effectsize random_seed
#> 1   (Intercept)    effect.power 0.6799517     30          1         123
#> 2          cost    effect.power 0.6799517     30          1         123
#> 3          size    effect.power 0.3767867     30          1         123
#> 4          type    effect.power 0.4478528     30          1         123
#> 5     cost:size    effect.power 0.3767867     30          1         123
#> 6     cost:type    effect.power 0.4478528     30          1         123
#> 7     size:type    effect.power 0.5799045     30          1         123
#> 8   (Intercept) parameter.power 0.6799517     30          1         123
#> 9          cost parameter.power 0.6799517     30          1         123
#> 10        size1 parameter.power 0.4020228     30          1         123
#> 11        size2 parameter.power 0.4020228     30          1         123
#> 12        type1 parameter.power 0.2809834     30          1         123
#> 13        type2 parameter.power 0.2809834     30          1         123
#> 14        type3 parameter.power 0.2961708     30          1         123
#> 15   cost:size1 parameter.power 0.4020228     30          1         123
#> 16   cost:size2 parameter.power 0.4020228     30          1         123
#> 17   cost:type1 parameter.power 0.2809834     30          1         123
#> 18   cost:type2 parameter.power 0.2809834     30          1         123
#> 19   cost:type3 parameter.power 0.2961708     30          1         123
#> 20  size1:type1 parameter.power 0.1750731     30          1         123
#> 21  size2:type1 parameter.power 0.1605014     30          1         123
#> 22  size1:type2 parameter.power 0.1605014     30          1         123
#> 23  size2:type2 parameter.power 0.1750731     30          1         123
#> 24  size1:type3 parameter.power 0.1645291     30          1         123
#> 25  size2:type3 parameter.power 0.1820507     30          1         123
#> 26  (Intercept)    effect.power 0.6957588     31          1         123
#> 27         cost    effect.power 0.6957588     31          1         123
#> 28         size    effect.power 0.3857370     31          1         123
#> 29         type    effect.power 0.4638559     31          1         123
#> 30    cost:size    effect.power 0.3857370     31          1         123
#> 31    cost:type    effect.power 0.4638559     31          1         123
#> 32    size:type    effect.power 0.6014444     31          1         123
#> 33  (Intercept) parameter.power 0.6957588     31          1         123
#> 34         cost parameter.power 0.6957588     31          1         123
#> 35        size1 parameter.power 0.4115382     31          1         123
#> 36        size2 parameter.power 0.4115382     31          1         123
#> 37        type1 parameter.power 0.2864825     31          1         123
#> 38        type2 parameter.power 0.3006589     31          1         123
#> 39        type3 parameter.power 0.3017406     31          1         123
#> 40   cost:size1 parameter.power 0.4115382     31          1         123
#> 41   cost:size2 parameter.power 0.4115382     31          1         123
#> 42   cost:type1 parameter.power 0.2864825     31          1         123
#> 43   cost:type2 parameter.power 0.3006589     31          1         123
#> 44   cost:type3 parameter.power 0.3017406     31          1         123
#> 45  size1:type1 parameter.power 0.1627166     31          1         123
#> 46  size2:type1 parameter.power 0.1627166     31          1         123
#> 47  size1:type2 parameter.power 0.1847808     31          1         123
#> 48  size2:type2 parameter.power 0.1847808     31          1         123
#> 49  size1:type3 parameter.power 0.1852767     31          1         123
#> 50  size2:type3 parameter.power 0.1667419     31          1         123
#> 51  (Intercept)    effect.power 0.7217557     32          1         123
#> 52         cost    effect.power 0.7114742     32          1         123
#> 53         size    effect.power 0.4056223     32          1         123
#> 54         type    effect.power 0.4940241     32          1         123
#> 55    cost:size    effect.power 0.4026865     32          1         123
#> 56    cost:type    effect.power 0.4843502     32          1         123
#> 57    size:type    effect.power 0.6174756     32          1         123
#> 58  (Intercept) parameter.power 0.7217557     32          1         123
#> 59         cost parameter.power 0.7114742     32          1         123
#> 60        size1 parameter.power 0.4220396     32          1         123
#> 61        size2 parameter.power 0.4380334     32          1         123
#> 62        type1 parameter.power 0.3134382     32          1         123
#> 63        type2 parameter.power 0.3087049     32          1         123
#> 64        type3 parameter.power 0.3073138     32          1         123
#> 65   cost:size1 parameter.power 0.4184315     32          1         123
#> 66   cost:size2 parameter.power 0.4313056     32          1         123
#> 67   cost:type1 parameter.power 0.3059611     32          1         123
#> 68   cost:type2 parameter.power 0.3059611     32          1         123
#> 69   cost:type3 parameter.power 0.3069861     32          1         123
#> 70  size1:type1 parameter.power 0.1828776     32          1         123
#> 71  size2:type1 parameter.power 0.1820009     32          1         123
#> 72  size1:type2 parameter.power 0.1863350     32          1         123
#> 73  size2:type2 parameter.power 0.1699729     32          1         123
#> 74  size1:type3 parameter.power 0.1685079     32          1         123
#> 75  size2:type3 parameter.power 0.1875840     32          1         123
#> 76  (Intercept)    effect.power 0.7438833     33          1         123
#> 77         cost    effect.power 0.7252330     33          1         123
#> 78         size    effect.power 0.4284742     33          1         123
#> 79         type    effect.power 0.5178300     33          1         123
#> 80    cost:size    effect.power 0.4213750     33          1         123
#> 81    cost:type    effect.power 0.5011963     33          1         123
#> 82    size:type    effect.power 0.6881126     33          1         123
#> 83  (Intercept) parameter.power 0.7438833     33          1         123
#> 84         cost parameter.power 0.7252330     33          1         123
#> 85        size1 parameter.power 0.4489231     33          1         123
#> 86        size2 parameter.power 0.4475224     33          1         123
#> 87        type1 parameter.power 0.3211385     33          1         123
#> 88        type2 parameter.power 0.3151385     33          1         123
#> 89        type3 parameter.power 0.3159419     33          1         123
#> 90   cost:size1 parameter.power 0.4392456     33          1         123
#> 91   cost:size2 parameter.power 0.4392456     33          1         123
#> 92   cost:type1 parameter.power 0.3106586     33          1         123
#> 93   cost:type2 parameter.power 0.3106586     33          1         123
#> 94   cost:type3 parameter.power 0.3106586     33          1         123
#> 95  size1:type1 parameter.power 0.1858696     33          1         123
#> 96  size2:type1 parameter.power 0.1728170     33          1         123
#> 97  size1:type2 parameter.power 0.1849216     33          1         123
#> 98  size2:type2 parameter.power 0.1840955     33          1         123
#> 99  size1:type3 parameter.power 0.1710522     33          1         123
#> 100 size2:type3 parameter.power 0.1895420     33          1         123
#> 101 (Intercept)    effect.power 0.7569160     34          1         123
#> 102        cost    effect.power 0.7376450     34          1         123
#> 103        size    effect.power 0.4313363     34          1         123
#> 104        type    effect.power 0.5390681     34          1         123
#> 105   cost:size    effect.power 0.4260546     34          1         123
#> 106   cost:type    effect.power 0.5185534     34          1         123
#> 107   size:type    effect.power 0.6984918     34          1         123
#> 108 (Intercept) parameter.power 0.7569160     34          1         123
#> 109        cost parameter.power 0.7376450     34          1         123
#> 110       size1 parameter.power 0.4515893     34          1         123
#> 111       size2 parameter.power 0.4545162     34          1         123
#> 112       type1 parameter.power 0.3273055     34          1         123
#> 113       type2 parameter.power 0.3308352     34          1         123
#> 114       type3 parameter.power 0.3553993     34          1         123
#> 115  cost:size1 parameter.power 0.4464771     34          1         123
#> 116  cost:size2 parameter.power 0.4464771     34          1         123
#> 117  cost:type1 parameter.power 0.3149170     34          1         123
#> 118  cost:type2 parameter.power 0.3149170     34          1         123
#> 119  cost:type3 parameter.power 0.3315543     34          1         123
#> 120 size1:type1 parameter.power 0.1883277     34          1         123
#> 121 size2:type1 parameter.power 0.1744749     34          1         123
#> 122 size1:type2 parameter.power 0.1748534     34          1         123
#> 123 size2:type2 parameter.power 0.1896316     34          1         123
#> 124 size1:type3 parameter.power 0.1979767     34          1         123
#> 125 size2:type3 parameter.power 0.1994969     34          1         123
#> 126 (Intercept)    effect.power 0.7565659     35          1         123
#> 127        cost    effect.power 0.7500965     35          1         123
#> 128        size    effect.power 0.4717278     35          1         123
#> 129        type    effect.power 0.5434100     35          1         123
#> 130   cost:size    effect.power 0.4438808     35          1         123
#> 131   cost:type    effect.power 0.5352030     35          1         123
#> 132   size:type    effect.power 0.7488180     35          1         123
#> 133 (Intercept) parameter.power 0.7565659     35          1         123
#> 134        cost parameter.power 0.7500965     35          1         123
#> 135       size1 parameter.power 0.4720082     35          1         123
#> 136       size2 parameter.power 0.4891384     35          1         123
#> 137       type1 parameter.power 0.3212784     35          1         123
#> 138       type2 parameter.power 0.3371426     35          1         123
#> 139       type3 parameter.power 0.3525940     35          1         123
#> 140  cost:size1 parameter.power 0.4536851     35          1         123
#> 141  cost:size2 parameter.power 0.4664493     35          1         123
#> 142  cost:type1 parameter.power 0.3190901     35          1         123
#> 143  cost:type2 parameter.power 0.3356799     35          1         123
#> 144  cost:type3 parameter.power 0.3356799     35          1         123
#> 145 size1:type1 parameter.power 0.1759507     35          1         123
#> 146 size2:type1 parameter.power 0.2003241     35          1         123
#> 147 size1:type2 parameter.power 0.2039876     35          1         123
#> 148 size2:type2 parameter.power 0.2082862     35          1         123
#> 149 size1:type3 parameter.power 0.1955240     35          1         123
#> 150 size2:type3 parameter.power 0.1972935     35          1         123
#> 151 (Intercept)    effect.power 0.7964338     36          1         123
#> 152        cost    effect.power 0.7627026     36          1         123
#> 153        size    effect.power 0.4884360     36          1         123
#> 154        type    effect.power 0.5903243     36          1         123
#> 155   cost:size    effect.power 0.4613127     36          1         123
#> 156   cost:type    effect.power 0.5512069     36          1         123
#> 157   size:type    effect.power 0.7469902     36          1         123
#> 158 (Intercept) parameter.power 0.7964338     36          1         123
#> 159        cost parameter.power 0.7627026     36          1         123
#> 160       size1 parameter.power 0.4964549     36          1         123
#> 161       size2 parameter.power 0.4964549     36          1         123
#> 162       type1 parameter.power 0.3655058     36          1         123
#> 163       type2 parameter.power 0.3575345     36          1         123
#> 164       type3 parameter.power 0.3655058     36          1         123
#> 165  cost:size1 parameter.power 0.4733657     36          1         123
#> 166  cost:size2 parameter.power 0.4733657     36          1         123
#> 167  cost:type1 parameter.power 0.3397409     36          1         123
#> 168  cost:type2 parameter.power 0.3397409     36          1         123
#> 169  cost:type3 parameter.power 0.3397409     36          1         123
#> 170 size1:type1 parameter.power 0.1988701     36          1         123
#> 171 size2:type1 parameter.power 0.2049134     36          1         123
#> 172 size1:type2 parameter.power 0.2008327     36          1         123
#> 173 size2:type2 parameter.power 0.2028465     36          1         123
#> 174 size1:type3 parameter.power 0.2049134     36          1         123
#> 175 size2:type3 parameter.power 0.1988701     36          1         123
#> 176 (Intercept)    effect.power 0.7852947     37          1         123
#> 177        cost    effect.power 0.7790389     37          1         123
#> 178        size    effect.power 0.5006448     37          1         123
#> 179        type    effect.power 0.5849562     37          1         123
#> 180   cost:size    effect.power 0.4648473     37          1         123
#> 181   cost:type    effect.power 0.5733489     37          1         123
#> 182   size:type    effect.power 0.7687754     37          1         123
#> 183 (Intercept) parameter.power 0.7852947     37          1         123
#> 184        cost parameter.power 0.7790389     37          1         123
#> 185       size1 parameter.power 0.5095474     37          1         123
#> 186       size2 parameter.power 0.5200584     37          1         123
#> 187       type1 parameter.power 0.3460287     37          1         123
#> 188       type2 parameter.power 0.3460287     37          1         123
#> 189       type3 parameter.power 0.3628078     37          1         123
#> 190  cost:size1 parameter.power 0.4819740     37          1         123
#> 191  cost:size2 parameter.power 0.4819740     37          1         123
#> 192  cost:type1 parameter.power 0.3445838     37          1         123
#> 193  cost:type2 parameter.power 0.3445838     37          1         123
#> 194  cost:type3 parameter.power 0.3445838     37          1         123
#> 195 size1:type1 parameter.power 0.2020064     37          1         123
#> 196 size2:type1 parameter.power 0.2063288     37          1         123
#> 197 size1:type2 parameter.power 0.2094348     37          1         123
#> 198 size2:type2 parameter.power 0.2076826     37          1         123
#> 199 size1:type3 parameter.power 0.2012961     37          1         123
#> 200 size2:type3 parameter.power 0.2067776     37          1         123
#> 201 (Intercept)    effect.power 0.8175931     38          1         123
#> 202        cost    effect.power 0.7962184     38          1         123
#> 203        size    effect.power 0.4993165     38          1         123
#> 204        type    effect.power 0.6185299     38          1         123
#> 205   cost:size    effect.power 0.4893051     38          1         123
#> 206   cost:type    effect.power 0.5947261     38          1         123
#> 207   size:type    effect.power 0.8156383     38          1         123
#> 208 (Intercept) parameter.power 0.8175931     38          1         123
#> 209        cost parameter.power 0.7962184     38          1         123
#> 210       size1 parameter.power 0.5131751     38          1         123
#> 211       size2 parameter.power 0.5140269     38          1         123
#> 212       type1 parameter.power 0.3590781     38          1         123
#> 213       type2 parameter.power 0.3730207     38          1         123
#> 214       type3 parameter.power 0.3956758     38          1         123
#> 215  cost:size1 parameter.power 0.4910267     38          1         123
#> 216  cost:size2 parameter.power 0.5103905     38          1         123
#> 217  cost:type1 parameter.power 0.3495862     38          1         123
#> 218  cost:type2 parameter.power 0.3495862     38          1         123
#> 219  cost:type3 parameter.power 0.3773299     38          1         123
#> 220 size1:type1 parameter.power 0.2015990     38          1         123
#> 221 size2:type1 parameter.power 0.2011486     38          1         123
#> 222 size1:type2 parameter.power 0.2095112     38          1         123
#> 223 size2:type2 parameter.power 0.2013861     38          1         123
#> 224 size1:type3 parameter.power 0.2121892     38          1         123
#> 225 size2:type3 parameter.power 0.2097445     38          1         123
#> 226 (Intercept)    effect.power 0.8187401     39          1         123
#> 227        cost    effect.power 0.8143658     39          1         123
#> 228        size    effect.power 0.5398129     39          1         123
#> 229        type    effect.power 0.6225871     39          1         123
#> 230   cost:size    effect.power 0.5131909     39          1         123
#> 231   cost:type    effect.power 0.6153382     39          1         123
#> 232   size:type    effect.power 0.8380883     39          1         123
#> 233 (Intercept) parameter.power 0.8187401     39          1         123
#> 234        cost parameter.power 0.8143658     39          1         123
#> 235       size1 parameter.power 0.5387913     39          1         123
#> 236       size2 parameter.power 0.5474098     39          1         123
#> 237       type1 parameter.power 0.3558576     39          1         123
#> 238       type2 parameter.power 0.3842160     39          1         123
#> 239       type3 parameter.power 0.3842160     39          1         123
#> 240  cost:size1 parameter.power 0.5191018     39          1         123
#> 241  cost:size2 parameter.power 0.5191018     39          1         123
#> 242  cost:type1 parameter.power 0.3548121     39          1         123
#> 243  cost:type2 parameter.power 0.3822490     39          1         123
#> 244  cost:type3 parameter.power 0.3822490     39          1         123
#> 245 size1:type1 parameter.power 0.2145179     39          1         123
#> 246 size2:type1 parameter.power 0.2151937     39          1         123
#> 247 size1:type2 parameter.power 0.2358854     39          1         123
#> 248 size2:type2 parameter.power 0.2201143     39          1         123
#> 249 size1:type3 parameter.power 0.2129932     39          1         123
#> 250 size2:type3 parameter.power 0.2145500     39          1         123
#> 251 (Intercept)    effect.power 0.8309356     40          1         123
#> 252        cost    effect.power 0.8303912     40          1         123
#> 253        size    effect.power 0.5461875     40          1         123
#> 254        type    effect.power 0.6403903     40          1         123
#> 255   cost:size    effect.power 0.5160100     40          1         123
#> 256   cost:type    effect.power 0.6390699     40          1         123
#> 257   size:type    effect.power 0.8390432     40          1         123
#> 258 (Intercept) parameter.power 0.8309356     40          1         123
#> 259        cost parameter.power 0.8303912     40          1         123
#> 260       size1 parameter.power 0.5478595     40          1         123
#> 261       size2 parameter.power 0.5521027     40          1         123
#> 262       type1 parameter.power 0.3916939     40          1         123
#> 263       type2 parameter.power 0.3846057     40          1         123
#> 264       type3 parameter.power 0.4018997     40          1         123
#> 265  cost:size1 parameter.power 0.5269844     40          1         123
#> 266  cost:size2 parameter.power 0.5269844     40          1         123
#> 267  cost:type1 parameter.power 0.3900963     40          1         123
#> 268  cost:type2 parameter.power 0.3900963     40          1         123
#> 269  cost:type3 parameter.power 0.3866960     40          1         123
#> 270 size1:type1 parameter.power 0.2186679     40          1         123
#> 271 size2:type1 parameter.power 0.2168117     40          1         123
#> 272 size1:type2 parameter.power 0.2171821     40          1         123
#> 273 size2:type2 parameter.power 0.2201919     40          1         123
#> 274 size1:type3 parameter.power 0.2152208     40          1         123
#> 275 size2:type3 parameter.power 0.2407802     40          1         123
#> No errors or warnings encountered during power curve generation!
```
