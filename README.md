# SMPR
SMPR Language R
# Метрические алгоритмы классификации
  Алгоритм классификации, основанный на вычислении оценок сходства между объектами. Простейшим метрическим классификатором является метод ближайших соседей, в котором классифицируемый объект относится к тому классу, которому принадлежит большинство схожих с ним объектов.

## Метод ближайших соседей
  
**Алгоритм ближайшего соседа** (nearest neighbor, NN) является самым простым
алгоритмом классификации. Он относит классифицируемый объект u ∈ Xℓ к тому
классу, которому принадлежит ближайший обучающий объект:

![](https://github.com/Ismailodabashi/SMPR/blob/master/2.gif)
       
Обучение NN сводится к запоминанию выборки Xℓ.

Для примера использования методов классификация была взята выборка ирисов фишера по длине и ширине лепестка.

![](https://github.com/Ismailodabashi/SMPR/blob/master/Ирисы%20Фишера.png)

## Метод kNN

**Алгоритм k ближайших соседей** (k nearest neighbors, kNN). Метрический алгоритм для автоматической классификации объектов.
Объект присваивается к тому классу, который является наиболее распространённым среди k соседей данного элемента, классы которых уже известны.


``` R
## Применяем метод kNN
kNN <- function(xl, z, k)
{
  ## Сортируем выборку согласно классифицируемого объекта
	orderedXl <- sortObjectsByDist(xl, z)
	n <- dim(orderedXl)[2] - 1

  ## Получаем классы первых k соседей
	classes <- orderedXl[1:k, n + 1]

  ##Составляем таблицу встречаемости каждого класса
	counts <- table(classes)

  ## Находим класс, который доминирует среди первых k соседей
	class <- names(which.max(counts))
	return (class)
} 
```

  <p>
    <img src="https://github.com/Ismailodabashi/SMPR/blob/master/Карта%20Классификации%201NN.png"  width="430" height="310">
    <img src="https://github.com/Ismailodabashi/SMPR/blob/master/Карта%20классификации%206NN.png"  width="430" height="310">
  </p>

### Преимущества:
1. Простота реализации.

### Недостатки:
1. Нужно хранить всю выборку.
2. При *k = 1* неустойчивость к погрешностям (*выбросам* -- объектам, которые окружены объектами чужого класса), вследствие чего этот выброс классифицировался неверно и окружающие его объекты, для которого он окажется ближайшим, тоже.
2. При *k = l* алгоритм наоборот чрезмерно устойчив и вырождается в константу.
3. Крайне бедный набор параметров.
4. Точки, расстояние между которыми одинаково, не все будут учитываться.

## LOO kNN

При k=1 алгоритм ближайшего соседа неустойчив к шумовым выбросам: он даёт ошибочные классификации не только на самих объектах-выбросах, но и на ближайших к ним объектах других классов. При k=m, наоборот, алгоритм чрезмерно устойчив и вырождается в константу. Таким образом, крайние значения k нежелательны. На практике оптимальное значение параметра k определяют по критерию скользящего контроля, чаще всего — методом исключения объектов по одному (leave-one-out cross-validation).

``` R
Loo <- function(k,xl)
{
	sum <- 0
	for(i in 1:dim(xl)[1]){
		if(i==1){
				tmpXL <- xl[2:dim(xl)[1],]
			}
			else if (i==dim(xl)[1]) {
				tmpXL <- xl[1:dim(xl)[1]-1,]
			}
			else {
					
				tmpXL <- rbind(xl[1:i-1, ], xl[i+1:dim(xl)[1],])
			}

		xi <- c(xl[i,1], xl[i,2])
		class <-kNN(tmpXL,xi,k)
		if(class != xl[i,3])
		sum <- sum+1
	}
	return(sum)
}

```

Для нашей задачи оптимально k=6, при этом значении алгоритм LOO показывает минимальное количество ошибок, по сравнению с другими значениями k.

![](https://github.com/Ismailodabashi/SMPR/blob/master/График%20зависимости%20LOO(k).png)

## Метод kwNN

**Метод взвешенных ближайших соседей.** В задачах с числом классов 3 и более нечётность уже не помогает, и ситуации неоднозначности всё равно могут возникать. Тогда i-му соседу приписывается вес w, как правило, убывающий с ростом ранга соседа i. Объект относится к тому классу, который набирает больший суммарный вес среди k ближайших соседей.

``` R
kwNN <- function(xl, z, k, q) 
{ 
  orderedXl <- sortObjectsByDist(xl, z) 
  n <- dim(orderedXl)[2] - 1 
  v1 <- c('setosa', 'versicolor', 'virginica')
  v2 <- c(0,0,0)
  
  for(i in 1:k){ 
    orderedXl[i, 4] = q^i 
  } 
  
  a=n+1 
  b=n+2 
  classes <- orderedXl[1:k, a:b]
  
  v2[1]=sum(classes[classes$Species=='setosa', 2])
  v2[2]=sum(classes[classes$Species=='versicolor', 2])
  v2[3]=sum(classes[classes$Species=='virginica', 2])
  
  amo <- cbind(v1,v2)
  
  class <- v1[which.max(v2)]
  return (class) 
}
```
![](https://github.com/Ismailodabashi/SMPR/blob/master/Карта%20классификации%20kwNN.png)

``` R
Loo <- function(k,q,xl)
{
  sum <- 0
  for(i in 1:dim(xl)[1]){
    if(i==1){
      tmpXL <- xl[2:dim(xl)[1],]
    }
    else if (i==dim(xl)[1]) {
      tmpXL <- xl[1:dim(xl)[1]-1,]
    }
    else {
      
      tmpXL <- rbind(xl[1:i-1, ], xl[i+1:dim(xl)[1],])
    }
    
    xi <- c(xl[i,1], xl[i,2])
    class <-kwNN(tmpXL,xi,k,q)
    if(class != xl[i,3])
      sum <- sum+1
  }
  return(sum)
}
```
Для нашей задачи оптимальными k являются от 3 до 150. При таких значениях k процент ошибок минимальный.

![](https://github.com/Ismailodabashi/SMPR/blob/master/График%20зависимости%20kwNN.png)

**Сравнение качества алгоритмов kNN и kwNN**

kNN — один из простейших алгоритмов классификации, поэтому на реальных задачах он зачастую оказывается неэффективным. Помимо точности классификации, проблемой этого классификатора является скорость классификации: если в обучающей выборке N объектов, в тестовой выборе M объектов, и размерность пространства K, то количество операций для классификации тестовой выборки может быть оценено как O(KMN).

kwNN отличается от kNN, тем что учитывает порядок соседей классифицируемого объекта, улчшая качество классификации.

Для демонстрации преимущества kwNN перед kNN, возьмём часть выборки Ирисов Фишера, и применим два метода:

  <p>
    <img src="https://github.com/Ismailodabashi/SMPR/blob/master/Сравнение%20kNN.png"  width="430" height="310">
    <img src="https://github.com/Ismailodabashi/SMPR/blob/master/Сравнение%20kwNN.png"  width="430" height="310">
  </p>

