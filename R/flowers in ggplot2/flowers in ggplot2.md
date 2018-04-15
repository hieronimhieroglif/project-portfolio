
## 1. Patterns in nature

<p>** <em>The scientist does not study nature because it is useful; he studies it because he delights in it, and he delights in it because it is beautiful (Henri Poincaré)</em> **</p>
<p>There are many examples of <em>natural facts</em> that can be described in mathematical terms. Nice examples are the shape of snowflakes, the <em>fractal geometry</em> of romanesco broccoli or how self-similarity rules the growth of plants.</p>
<p>R is a tool for doing serious analysis, but not everything in life is serious. Life is also funny, and R can be used to have fun and to do beautiful things. Its graphical power can be used to produce artistic images like the one that illustrates this section, which is inspired by how plants arrange their leaves. This fact is called <em>phyllotaxis</em> and will serve as the basis of this project.</p>
<p>In this notebook, we are using the <code>ggplot2</code> package. Apart from having fun, we will learn many important features of it that will be useful not only to do art but also to represent data in real-life problems. Let's start by loading the library.</p>


```R
# This sets plot images to a nice size.
options(repr.plot.width = 4, repr.plot.height = 4)

library(ggplot2)
```


## 2. Warming up: drawing points on a circle
<p>There are many ways to represent data with <code>ggplot2</code>: from simple scatter plots to more complex ones, such as <em>violin</em> plots. The functions that start with <code>geom_</code> define how the plot is shown. In this notebook, we will only work with <code>geom_point</code> which plots points in two dimensions. We just need a dataset with two variables, let's call them <code>x</code> and <code>y</code>.</p>
<p>Let's start by drawing 50 points on a circle of radius 1. As every <code>(x, y)</code> point should be in the unit circle, it follows that x² + y² = 1. We can get this using the <em>superfamous</em> Pythagorean trigonometric identity which states that sin²(θ) + cos²(θ) = 1 for any real number θ.</p>


```R
t <- seq(0, 2*pi, length.out = 50)
x <- sin(t)
y <- cos(t)
df <- data.frame(t, x, y)

# Scatter plot of points in a circle
p <- ggplot(df, aes(x, y))
p + geom_point()
```




![png](output_4_1.png)




## 3. Make it harmonious with the Golden Angle
<p>Plants arrange their leaves in spirals. A spiral is a curve starts from the origin and <em>moves away</em> from this point as it revolves around it. In the plot above all our points are at the same distance from the origin. A simple way to arrange them in a spiral is to multiply <code>x</code> and <code>y</code> by a factor which increases for each point. We <em>could</em> use <code>t</code> as that factor, as it meets these conditions, but we will do something more <em>harmonious</em>. We will use the <a href="https://en.wikipedia.org/wiki/Golden_angle">Golden Angle</a>:</p>
<p>Golden Angle = π(3 − √5)</p>
<p>This number is inspired by the Golden Ratio, one of the most famous numbers in the history of mathematics. Both the Golden Ratio and the Golden Angle appear in unexpected places in nature. Apart of flower petals and plant leaves, you'll find them in seed heads, pine cones, sunflower seeds, shells, spiral galaxies, hurricanes, etc.</p>
<p>It's time to <em>spiralize</em>!</p>


```R
# Defining the number of points
points <- 500

# Defining the Golden Angle
angle <- pi*(3-sqrt(5))

t <- (1:points) * angle
x <- sin(t)
y <-cos(t)
df <- data.frame(t, x, y)

# Scatter plot of points in a spiral
p <- ggplot(df, aes(x*t, y*t))
p + geom_point()
```




![png](output_7_1.png)




## 4. Remove everything unnecessary
<p>Apart from data, a plot includes many other components that define its final appearance. Our previous plot contains:</p>
<ul>
<li>a <strong>background</strong> filled with grey color.</li>
<li>a <strong>grid</strong> of horizontal and vertical white lines in the background.</li>
<li><strong>ticks</strong> along the axis.</li>
<li>a <strong>title</strong> on each axis.</li>
<li><strong>text</strong> along axes to label marks.</li>
</ul>
<p>Art does not get along with most of these elements, so it's time to move to action.</p>


```R
df <- data.frame(t, x, y)

# Scatter plot of points in a spiral
p <- ggplot(df, aes(x*t, y*t))
p + geom_point() +
theme(panel.background=element_rect(fill="white"), panel.grid=element_blank(), 
     axis.ticks=element_blank(), title=element_blank(), text=element_blank())
```




![png](output_10_1.png)



```R
# checking if one of the this are blank, 
# seems too much to check them all...

run_tests({
    test_that("Background is white.", {
    expect_equal(last_plot()$theme$panel.background$fill, "white", 
        info = "The background should be white.")
    })
    test_that("Grid is removed.", {
    expect_true("element_blank" %in% class(last_plot()$theme$panel.grid), 
        info = "The grid lines should be removed.")
    })
})
```


## 5. A bit of makeup: size, color and transparency
<p>Our drawing starts to look like a plant, but we can do it much better. By changing color, transparency (also called <em>alpha</em>), and size of the points, the image will become more appealing.</p>


```R
p <- ggplot(df, aes(x*t, y*t))
p + geom_point(size=8, alpha=0.5, color="darkgreen") +
theme(panel.background=element_rect(fill="white"), panel.grid=element_blank(), 
     axis.ticks=element_blank(), title=element_blank(), text=element_blank())
```




![png](output_13_1.png)




## 6. Play with aesthetics: the dandelion
<p>Until now, all points have the same appearance (<code>size</code>, <code>color</code>, <code>shape</code>, and <code>alpha</code>). Sometimes one will want to make the appearance of the points dependent on a variable in your dataset. Now I will make size variable. I will also change the shape of points. Although I won't be able to blow on it, the resulting image should remind you of a dandelion.</p>


```R
p <- ggplot(df, aes(x*t, y*t))
p + geom_point(aes(size=t), alpha=0.5, color="black", shape=8) +
theme(panel.background=element_rect(fill="white"), panel.grid=element_blank(), 
     axis.ticks=element_blank(), title=element_blank(), text=element_blank(),
     legend.position="none")
```




![png](output_16_1.png)



## 7. Put all it together: the sunflower
<p>Plants not only use the Golden Angle to arrange leaves. It is also found in the arrangement of sunflower seeds.</p>


```R
p <- ggplot(df, aes(x*t, y*t))
p + geom_point(aes(size=t), alpha=0.5, color="yellow", shape=17) +
theme(panel.background=element_rect(fill="darkmagenta"), panel.grid=element_blank(), 
     axis.ticks=element_blank(), title=element_blank(), text=element_blank(),
     legend.position="none")
```




![png](output_19_1.png)



## 8. What if you modify the angle?
<p>These patterns are very sensitive to the angle between the points that form the spiral; small changes to the angel can generate very different images. Let's look at an example of that.</p>


```R
angle <- 2
points <- 1000

t <- (1:points)*angle
x <- sin(t)
y <- cos(t)

df <- data.frame(t, x, y)

p <- ggplot(df, aes(x*t, y*t))
p + geom_point(aes(size=t), alpha=0.5, color="yellow", shape=17) +
theme(panel.background=element_rect(fill="darkmagenta"), panel.grid=element_blank(), 
     axis.ticks=element_blank(), title=element_blank(), text=element_blank(),
     legend.position="none")
```




![png](output_22_1.png)




## 9. All together now: imaginary flowers

<p>The techniques shown so far allows to create an <em>infinite</em> number of patterns inspired by nature: the only limit is your imagination. But making art has also been a fun excuse to learn to use <code>ggplot</code>. All the tricks used in this notebook are useful when plotting <em>real</em> data too.</p>

```R
angle <- 13*pi/180
points <- 2000

t <- (1:points)*angle
x <- sin(t)
y <- cos(t)

df <- data.frame(t, x, y)

p <- ggplot(df, aes(x*t, y*t))
p + geom_point(aes(size=80), alpha=0.1, color="magenta4", shape=1) +
theme(panel.background=element_rect(fill="white"), panel.grid=element_blank(), 
     axis.ticks=element_blank(), title=element_blank(), text=element_blank(),
     legend.position="none")
```




![png](output_25_1.png)
