# Методы принятия решений
Работы подготовила: Балицкая Анастсия 401-И
## Навигация

- [Метрические алгоритмы классификации](#Метрические-алгоритмы-классификации)
  - [Алгоритм 1NN и K ближайших соседей (KNN)](#алгоритм-1nn-и-k-ближайших-соседей-knn)
  - [K взвешенных ближайших соседей (kwKNN)](#k-взвешенных-ближайших-соседей-kwknn)
  - [Парзеновское окно (PW)](#Парзеновское-окно-pw)
  - [Потенциальные функции (PF)](#Потенциальные-функции-pf)
  - [STOLP](#stolp)
- [Байесовские классификаторы](#Байесовские-классификаторы)
 - ["Наивный" байесовский классификатор](#"Наивный"-байесовский-классификатор)
  
# Метрические алгоритмы классификации
**Метрические методы обучения** -- методы, основанные на анализе сходства объектов.

**_Мерой близости_** называют функцию расстояния ![](http://latex.codecogs.com/svg.latex?%5Clarge%20%5Crho%3A%20%28X%20%5Ctimes%20X%29%20%5Crightarrow%20%5Cmathbb%7BR%7D). Чем меньше расстояние между объектами, тем больше объекты похожи друг на друга.

Метрические алгоритмы классификации опираются на **_гипотезу компактности_**: схожим объектам соответствуют схожие ответы.

Метрические алгоритмы классификации с обучающей выборкой *Xl* относят объект *u* к тому классу *y*, для которого **суммарный вес ближайших обучающих объектов ![](https://latex.codecogs.com/gif.latex?W_y%28u%2C%20X%5El%29) максимален**:

![](https://latex.codecogs.com/gif.latex?W_y%28u%2C%20X%5El%29%20%3D%20%5Csum_%7Bi%20%3A%20y_%7Bu%7D%5E%7B%28i%29%7D%20%3D%20y%7D%20w%28i%2C%20u%29%20%5Crightarrow%20max)

, где весовая функция *w(i, u)* оценивает степень важности *i*-го соседа для классификации объекта *u*.

Функция ![](https://latex.codecogs.com/gif.latex?W_y%28u%2C%20X%5El%29) называется **_оценкой близости объекта u к классу y_**. Выбирая различную весовую функцию *w(i, u)* можно получать различные метрические классификаторы.

Для поиска оптимальных параметров для каждого из рассматриваемых ниже метрических алгоритмов используется **LOO -- leave-one-out** *(критерий скользящего контроля)*, который состоит в следующем: 

1. Исключать объекты *x(i)* из выборки *Xl* по одному, получится новая выборка без объекта *x(i)* (назовём её *Xl_1*).
2. Запускать алгоритм от объекта *u*, который нужно классифицировать, на выборке *Xl_1*.
3. Завести переменную *Q* (накопитель ошибки, изначально *Q = 0*) и, когда алгоритм ошибается, *Q = Q + 1*.
4. Когда все объекты *x(i)* будут перебраны, вычислить *LOO = Q / l* (*l* -- количество объектов выборки).

При минимальном значении LOO получим оптимальный параметр алгоритма.
### Алгоритм 1NN и K ближайших соседей (KNN)

**1NN:**

1. Подбирается метрика. В данной работе это декартово расстояние между векторами.
2. Обучающая выборка сортируется в порядке увеличения расстояния от классифицируемого элемента.
3. Элемент относят к тому классу к которому принадлежит ближайший (первый в отсортированной выборке) элемент.

**КNN:**

Имеется некоторая выборка *Xl*, состоящая из объектов *x(i), i = 1, ..., l* (в приложенной программе используется выборка ирисов Фишера).
Данный алгоритм классификации относит классифицируемый объект *u* к тому классу *y*, к которому относится большинство из *k* его ближайших соседей *x(u_i)*.
Для оценки близости классифицируемого объекта *u* к классу *y* **алгоритм kNN** использует следующую функцию:
![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20%5Bi%20%5Cleq%20k%5D) , где *i* -- порядок соседа по расстоянию к классифицируемому объекту *u*.

Реализация алгоритмов:

``` R
par(mfrow=c(2,1))
selected = data[c(1,3,5)]
features = dim(selected)[2]-1
cases = dim(selected)[1]
colors = c("red", "green", "blue")

dist = function(u, v) { #это возвращает эвклидовое расстояние между двумя объектами
  sqrt(sum((u-v)^2))
}

distances = function(obj, data, metric) { # это возвращает отсортированный набор данных по метрике для объекта  
dists = matrix(0, cases, 2)
  for (i in 1:cases) {
    cost = metric(obj, data[i,1:features])
    dists[i,] = c(cost, i)
  }
  idx = order(dists[,1])
  data[dists[idx,2],]
}

NN = function(obj, data, metric=dist) { # это 1-ближайший сосед  
sorted = distances(obj, data, metric)
  sorted[1,features+1]
}

kNN = function(obj, data, k, metric=dist) { # это k-ближайших соседей  
sorted = distances(obj, data, metric)
  
  n = 10 
  counts = rep(0, times=n)
  for (i in 1:k) {
    cls = sorted[i,features+1]
    counts[cls] = counts[cls] + 1
  }
  argmax = 1
  for (i in n) {
    if (counts[argmax] < counts[i]) {
      argmax = i
    }
  }
  
  cls[argmax]
}


points = rbind(#классификация 
  c(5.5, 2),
  c(6.5, 4),
  c(7, 6.5),
  c(5.4, 2.5)
)


# 1NN
plot(selected[,1], selected[,2], col=colors[selected[,features+1]], xlab="1NN", ylab="")
for (i in 1:dim(points)[1]) {
  pt = points[i,]
  points(pt[1], pt[2], col=colors[NN(pt, selected)], pch=19) 
}

# kNN
plot(selected[,1], selected[,2], col=colors[selected[,features+1]], xlab="kNN", ylab="")
for (i in 1:dim(points)[1]) {
  pt = points[i,]
  points(pt[1], pt[2], col=colors[kNN(pt, selected, 7)], pch=19) 
}
```
Вот что получилось:
![alt text](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/1_NN_kNN/результат%20рисунки.png)

Алгоритм kNN выглядит более качествеено. Для того чтобы привести более точное обоснование чем kNN лучше в этом случае, чем 1NN, следует прибегнуть к скользящему контролю.

**LOO для КNN:**

Посмотрим как отработал KNN при помощи алгоритма скользящего конторля - LOO.
![alt text](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/2_Loo_kNN/результат.png)

Минимальный LOO достигается при k=6

Програмная реализация:
``` R
getLoo = function(x) {
  l = dim(x)[1]
  n = dim(x)[2] - 1
  maxk = l
  
  loo = rep(0, times=maxk)
  
  for (i in 1:l) {
    dists = distances(x[i,], x[-i,], dist)
    for (k in 1:maxk) {
      class = applykNN(dists, k)
      if (as.integer(class) != as.integer(x[i,n+1])) {
        loo[k] = loo[k] + 1
      }
    }
    print(i)
    print(loo)
  }
  loo = loo / l
  return(loo)
}
which.min(res)
# res = getLoo(selected)
```
### Преимущества:
1. Простота реализации.
2. При *k*, подобранном около оптимального, алгоритм "неплохо" классифицирует.

### Недостатки:
1. Нужно хранить всю выборку.
2. При *k = 1* неустойчивость к погрешностям (*выбросам* -- объектам, которые окружены объектами чужого класса), вследствие чего этот выброс классифицировался неверно и окружающие его объекты, для которого он окажется ближайшим, тоже.
2. При *k = l* алгоритм наоборот чрезмерно устойчив и вырождается в константу.
3. Максимальная сумма объектов в *counts* может достигаться в нескольких классах одновременно.
4. "Скудный" набор параметров.
5. Точки, расстояние между которыми одинаково, не все будут учитываться.


### K взвешенных ближайших соседей (kwKNN)
Имеется некоторая выборка *Xl*, состоящая из объектов *x(i), i = 1, ..., l* (в приложенной программе используется выборка ирисов Фишера).
Данный алгоритм классификации относит объект *u* к тому классу *y*, у которого максимальна сумма весов *w_i* его ближайших *k* соседей *x(u_i)*.

Для оценки близости классифицируемого объекта *u* к классу *y* **алгоритм wkNN** использует следующую функцию:

![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20%5Bi%20%5Cleq%20k%5D%20w%28i%29) , где *i* -- порядок соседа по расстоянию к классифицируемому объекту *u*, а *w(i)* -- строго убывающая функция веса, задаёт вклад i-го соседа в классификацию.

В приложенной программе используется весовая функция вида: ![](https://latex.codecogs.com/gif.latex?w%28i%29%20%3D%20q%5Ei%2C%20q%20%5Cepsilon%20%280%2C%201%29)

Реализация kwKNN производится следующим образом:
``` R
kwNN = function(u, xl, k, q, ro) {
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  sorted = distances(u, xl, ro)
  facts = levels(xl[,n+1])
  scores = rep(0, times=length(facts))
  curr = 1
  for (i in 1:k) {
    currclass = sorted[i,n+1]
    scores[currclass] = scores[currclass] + curr
    curr = curr * q
  }
  max = which.max(scores)
  factor(facts[max], levels=facts)
}
```
Как в kNN внутри цикла получим отсортированных соседей, только теперь у нас будет ещё и множитель curr, который будет умножаться на i-того соседа. Во время каждой итерации curr = q^i.

Подобираем параметр q, k
Взяли k = 6, так как он показал себя наилучший LOO в kNN, а параметр q подбирать через LOO.
Наилучшим q оказался 1 с loo(q) = 0.0333.

Посмотрим как отработал kwKNN

![alt text](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/4_kwNN/map.png)

Из-за того, что **алгоритм k взвешенных ближайших соседей** учитывает порядок объектов при классификации, он выдаёт лучший результат, чем **алгоритм k ближайших соседей (kNN)**. Следовательно, объекты *x_i*, которые находятся ближе к классифицируемому объекту *u*, будут оказывать намного большее влияние, чем те *x_i*, которые дальше (из-за учёта порядка объектов).

### Преимущества:
1. Простота реализации.
2. При любом *k* алгоритм "неплохо" классифицирует.

### Недостатки:
1. Приходится хранить обучающую выборку *Xl* целиком, что приводит к неэффективному расходу памяти. При наличии погрешностей это может привести к понижению точности классификации вблизи границ классов.
2. Поиск ближайшего соседа предполагает сравнение классифицируемого объекта *u* со всеми объектами выборки *Xl (x(i), i = 1, ..., l)* за *O(l)* операций. Эта проблема решается с помощью эффективных алгоритмов за *O(lnl)*.
3. Исключается настройка алгоритмов по данным (крайне "бедный" набор параметров).
4. Если суммарные веса классов оказываются одинаковыми, то алгоритм относит классифицируемый объект *u* к любому из классов.

### Парзеновское окно (PW)

Для оценки близости объекта _u_ к классу _y_ алгоритм использует следующую
функцию:

![](http://latex.codecogs.com/svg.latex?%5Clarge%20W%28i%2C%20u%29%20%3D%20K%28%5Cfrac%7B%5Crho%28u%2C%20x%5Ei_u%29%7D%7Bh%7D%29)
, где 
![](http://latex.codecogs.com/svg.latex?%5Clarge%20K%28z%29) — функция ядра.

Чаще всего применяются 5 типов ядер:
- Прямоугольное ![](http://latex.codecogs.com/svg.latex?%5Clarge%20R%28z%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5B%7Cz%7C%20%5Cleq%201%5D)
- Треугольное ![](http://latex.codecogs.com/svg.latex?%5Clarge%20T%28z%29%20%3D%20%281%20-%20%7Cz%7C%29%20%5Ccdot%20%5B%7Cz%7C%20%5Cleq%201%5D)
- Квартическое ![](http://latex.codecogs.com/svg.latex?%5Clarge%20Q%28z%29%20%3D%20%5Cfrac%7B15%7D%7B16%7D%20%281%20-%20z%5E2%29%5E2%20%5Ccdot%20%5B%7Cz%7C%20%5Cleq%201%5D)
- Епанечниково ![](http://latex.codecogs.com/svg.latex?%5Clarge%20E%28z%29%20%3D%20%5Cfrac%7B3%7D%7B4%7D%20%281%20-%20z%5E2%29%20%5Ccdot%20%5B%7Cz%7C%20%5Cleq%201%5D)
- Гауссовское (нормальное распределение)

Реализация алгоритма:
``` R
a = function(u, xl, h, K, ro) {
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  facts = levels(xl[,n+1])
  s = rep(0, times=length(facts))
  for (i in 1:l) {
    fact = xl[i,n+1]
    score = K(ro(u, xl[i,1:n])/h)
    s[fact] = s[fact] + score
  }
  factor(facts[which.max(s)], levels=facts)
}
```
но тут возникает такая проблема(долго с ней разбирались):

![](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/3_ParzenWindow/problem.png)

Она возникла потому, что примеру брали точку (1000,1000) каждый класс получал очки (0,0,0) и алгоритм просто брал первый 0, поэтому в таком случае не будем классифицировать:

``` R
if (sum(s) == 0) {
  -1
} else {
  factor(facts[which.max(s)], levels=facts)
}
```

Тогда получим:

![](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/3_ParzenWindow/fixed_map.png)

Подобираем параметр «ширина окна».
Надо выбрать такое h, которое даст меньше всего ошибок. Для этого воспользуемся LOO:

Построим LOO(h) для 3х ядер: прямоугольное, треугольное и гауссовское:

![](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/3_ParzenWindow/loos.png)

Попробуем алгоритм на разных ядрах, сравниваем их, строим карты классификации. Результатами LOO оказались:

![](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/3_ParzenWindow/tabl.PNG)

Ошибка оказалась у всех ядер одинаковая. 

Посмотрим на карты классификации для разных ядер:
![](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/3_ParzenWindow/maps.png)

Вывод: Больше всего подходи гауссовское ядро. Оно однозначно разделило классы на всей плоскости. 

### Преимущества:
1. Простота реализации.
2. Все точки, попадающие в окно, расстояние между которыми одинаково, будут учитываться (в отличие от алгоритма *взвешенного kNN*).
3. Скорость классификации быстрее, т.к. не требуется сортировка расстояний (*O(l)*).
4. Окно с переменной шириной решает проблему разрешимости задач, в которых обучающие выборки распределены неравномерно по пространству *X* (как ирисы Фишера).
5. Выбор финитного ядра позволяет свести классификацию объекта *u* к поиску *k* его ближайших соседей, тогда как при не финитном ядре (например, гауссовском) нужно перебирать всю обучающую выборку *Xl*, что может приводить к неприемлемым затратам времени (при большом *l*).

### Недостатки:
1. Слишком узкие окна приводят к неустойчивой классификации, а слишком широкие - к вырождению алгоритма в константу.
2. Диапазон, из которого выбирается параметр *h*, нужно подбирать самим.
3. "Скудный" набор параметров.
4. Если в окно, радиуса *h*, не попало ни одной точки *x_i*, то алгоритм не способен классифицировать объект *u*.
5. Если суммарные веса классов оказываются одинаковыми, то алгоритм относит классифицируемый объект *u* к любому из классов.

### Потенциальные функции (PF)

Имеется некоторая выборка *Xl*, состоящая из объектов *x(i), i = 1, ..., l* (в приложенной программе используется выборка ирисов Фишера). В данном алгоритме весовая функция *w_i* определяется как функция от расстояния между классифицируемым объектом *u* и его соседями *x(u_i), i = 1, ..., l*, как и в **методе парзеновского окна**.

Для оценки близости объекта _u_ к классу _y_ алгоритм использует следующую
функцию:

![](http://latex.codecogs.com/svg.latex?W_y%28i%2C%20u%29%20%3D%20%5Cgamma_i%20%5Ccdot%20K%28%5Cfrac%7B%5Crho%28u%2C%20x_u%5Ei%29%7D%7Bh_i%7D%29%2C%20%5Cgamma_i%20%5Cgeq%200%2C%20h_i%20%3E%200)
, где 
![](http://latex.codecogs.com/svg.latex?%5Clarge%20K%28z%29) — функция ядра.

**Основная идея:** *Потенциалы* определяют важность каждого объекта *x_i* при классификации. Считаем, что радиусы потенциалов *h* известны заранее. Алгоритм подбирает только потенциалы ![](https://latex.codecogs.com/gif.latex?%5Cgamma_%7Bi%7D).

Изменяем  в парзеновском окне 1 строку, чтобы получить алгоритм потенциальных функций:

``` R 
score = g[i] * K(ro(u, xl[i,1:n])/h[i])
```
и алгоритм готов

Подобираем параметры h и gamma:
Взяли показатели  LOO в парзеновском окне  h[i] =0.5333 (гауссовское), 1.008 (квадратичное) и 0.509 (треугольное):
Выбираем gamma:
``` R 
g = rep(0, times=l)
cnt = 0
while (!(g[which.max(g)] >= 7 || cnt >= 20)) {
  for (i in 1:l) {
    class = pfunc(xl[i,1:n], xl[-i,], g, h, gaussian, dist)
    if (class != xl[i,n+1]) {
      g[i] = g[i] + 1
    }
  }
  cnt = cnt + 1
}
```

Построим карту классификации с потенциалами:
![](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/5_PotentialFunctions/Potentials.png)

Для квадратичного ядра алгоритм не сошёлся, ошибка составила 10%:

![](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/5_PotentialFunctions/map_square.png)

Для треугольного ядра ошибка достигла минимального значения – 0,666% была совершена только одна ошибка:

![](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/5_PotentialFunctions/map_triangle.png)

Посмотрим сравнительную таблицу:

![](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/5_PotentialFunctions/tabl.PNG)

Вывод:Треугольное ядро показало нормальную погрешность, нужно использовать его.

### Преимущества:
1. Достаточно богатый набор из *2l* параметров.

### Недостатки:
1. Результат классификации зависит от удачного подбора *h*, а они задаются вне алгоритма.
2. Нужно хранить целиком всю выборку.
3. Количество итераций алгоритма неизвестно, т.к. объекты *x_i* выбираются случайным образом.
4. Время работы алгоритма дольше предыдущих (из-за подсчёта ошибок сложность каждой итерации составляет *O(l^2)*, а таких итераций неизвестное количество).
4. Если в окно, радиуса *h*, не попало ни одной точки *x_i*, то алгоритм не способен классифицировать объект *u*.


### STOLP

Выделяют несколько выдов объектов обучения:

- _Эталонные_ — типичные представители классов. Если классифицируемый
объект близок к эталону, то, скорее всего, он принадлежит тому же классу.
- _Неинформативные_ — плотно окружены
другими объектами того же класса. Если их удалить из выборки, это практически
не отразится на качестве классификации.
- _Выбросы_ — находятся в окружении объектов чужого класса. Как правило,
- их удаление только улучшает качество классификации.

Алгорим **STOLP** исключает из выборки выбросы и неинформативные
объекты, оставляя лишь нужное количество эталонных. Таким образом
улучшается качество классификации, сокращается объем данных и уменьшается
время классификации объектов. Другими словами **STOLP** — алгоритм
сжатия данных.

Он использует функцию отступа:

![](http://latex.codecogs.com/svg.latex?M%28x_i%29%20%3D%20W_%7By_i%7D%28x_i%29%20-%20%5Cunderset%7By%20%5Cin%20Y%20%5Csetminus%20y_i%7D%7Bmax%7DW_y%28x_i%29%29%29)

![](http://latex.codecogs.com/svg.latex?W_y%28x_i%29) является весовой
функцией и зависит от выбранного алгоритма классификации.

Посмотрим на отсортированный список значений M(x) для всех элементов:

![](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/6_STOLP/mplot.png)

Элементы помеченные красным – выбросы. Алгоритм удалит их на самой начальной стадии:
``` R
good = rep(T, times=l)
for (i in 1:l) {
  print(sprintf("removing bad: %d", i))
  if (M(xl[i,1:n], xl[i,n+1], xl[-i,], k) < delta) {
    good[i] = F
  }
}
xl = xl[good,]
l = dim(xl)[1]
``` 

После чего в выборку войдут самые эталонные объекты с максимальным отступом:

``` R
inomega = rep(F, times=l)
classes = length(levels(xl[,n+1]))
idxval = matrix(0, nrow=classes, ncol=2)
for (i in 1:l) {
  print(sprintf("inital omega: %d", i))
  curr = M(xl[i,1:n], xl[i,n+1], xl[-i,], k)
  cls = xl[i,n+1]
  if (idxval[cls,2] < curr) {
    idxval[cls,1] = i
    idxval[cls,2] = curr
  }
}
```
И потом присоединяются объекты, на которых происходит ошибка:

``` R
omega = xl[inomega,]
xl_omega = xl[!inomega,]
worst = matrix(1e9, nrow=l, ncol=2)
for (i in 1:l) {
  print(sprintf("omega length: %d, current i: %d", length(inomega[inomega==T]), i))
  if (!inomega[i]) {
    val = M(xl[i,1:n], xl[i,n+1], omega, k)
    if (val < delta) {
      worst[i,] = c(i, val)
    }
  }
}
```

Посмотрим на выбранные объекты:

![](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/6_STOLP/maps.png)

![](https://github.com/BalitskayaNastya/Machine_learning_BalitskayaNastya/blob/master/Метрические%20классификаторы/6_STOLP/picked.png)

Видно что алгоритм правильно выбрал эталоны и граничные объекты.

Сравним точность на полной выборке и выборке STOLP:

Полная выборка: 4%

Выборка STOLP: 4%

Погрешность не уменьшилась, но поскольку алгоритм оставил только 19 объектов то kNN будет работать в 8 раз быстрее.

Вывод: В выборке есть эталоны и выбросы, чтобы удалить выбросы и взять эталоны и граничные объекты мы используем алгоритм STOLP. Поскольку остается меньше объектов метрические алгоритмы классификации работают быстрее и есть шанс, что ошибка уменьшится.

### Преимущества:
1. В разы уменьшает размер выборки.
2. Способен улучшить качество классификации.

### Недостатки:
1. Сложный в реализации.

## Байесовские классификаторы

**Байесовские алгоритмы классификации** основаны на предположении, что есть вероятностное пространство ![](https://latex.codecogs.com/gif.latex?X%20%5Ctimes%20Y) с неизвестной плотностью распределения ![](https://latex.codecogs.com/gif.latex?%5Crho%20%28x%2C%20y%29%20%3D%20P%28y%29%5Crho%20%28x%20%7C%20y%29), из которого случайно и независимо извлекаются *l* наблюдений.

Байесовский подход опирается на теорему о том, что **если плотности распределения классов известны, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде**.

Обозначим *величину потери* алгоритмом *а* при неправильной классификации объекта класса *y* ![](https://latex.codecogs.com/gif.latex?%5Clambda%20_%7By%7D).

**Теорема:** Если известны априорные вероятности классов *P(y)* и функции правдоподобия *p(x*|*y)*, то минимум среднего риска достигается алгоритмом ![](https://latex.codecogs.com/gif.latex?a%28x%29%20%3D%20arg%20%5Cmax_%7By%20%5Cepsilon%20Y%7D%5Clambda%20_%7By%7DP%28y%29%5Crho%28x%7Cy%29). Алгоритм *a(x)* называется **оптимальным байесовским решающим правилом**.

На практике зачастую плотности распределения классов неизвестны и их приходится восстанавливать по обучающей выборке. **Чем лучше удастся восстановить функции правдоподобия, тем ближе к оптимальному будет построенный алгоритм**.

В зависимости от способов восстановления плотности существует большое разнообразие **байесовских алгоритмов классификации**.

### "Наивный" байесовский классификатор
