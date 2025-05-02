import csv
import numpy

import numpy as np
import random
from scipy.special import expit


# Level-Based Learning PSO
class PSO():

    def __init__(self, pN, dim, max_iter, numberOfLayers_list, actual_data):
        self.r1 = 0
        self.r2 = 0
        self.r3 = 0

        self.upper = 10
        self.lower = 1e-10

        self.pN = pN
        self.dim = dim
        self.max_iter = max_iter
        self.X = np.zeros((self.pN, self.dim))
        self.V = np.zeros((self.pN, self.dim))
        self.pBest = np.zeros((self.pN, self.dim))
        self.gBest = np.zeros((1, self.dim))
        self.p_fit = np.zeros(self.pN)
        self.fit = 1e20

        self.actual_data = actual_data

        self.numberOfLayers_list = numberOfLayers_list
        self.qTable = np.zeros(
            (len(self.numberOfLayers_list), len(self.numberOfLayers_list)))
        self.preState = 0
        self.currentState = 0

        self.layers = []
        self.fitness = []

        self.phi = 0.4

        self.epsilon = 0.9
        self.alpha = 0.4
        self.gamma = 0.8

        self.iteratorMse = []

        self.CW = [row[0] for row in self.actual_data]
        self.CLR = [row[1] for row in self.actual_data]
        self.SD = [row[2] for row in self.actual_data]
        self.D = [row[3] for row in self.actual_data]
        self.PLOT = [row[4] for row in self.actual_data]

    def get_actual_data(self):
        return self.actual_data

    def function(self, X):
        a1 = X[0]
        a2 = X[1]
        b1 = X[2]
        c1 = X[3]
        c2 = X[4]

        CW_NEW = []

        for i in range(len(self.CW)):

            numerator = (a1 + a2 * self.CLR[i])
            denominator = 1 + b1 * expit(-(c1 + c2 * self.SD[i]) * self.D[i])
            cw = numerator / denominator
            CW_NEW.append(cw)

        mse = np.mean((np.array(CW_NEW) - np.array(self.CW))**2)
        return mse

    def selectAction(self):
        # 根据当前状态选择一个动作
        if np.random.rand() < self.epsilon:
            nextAction = np.argmax(self.qTable[self.currentState])
        else:
            nextAction = np.random.randint(0, len(self.numberOfLayers_list))

        self.preState = self.currentState
        self.currentState = nextAction
        return self.numberOfLayers_list[nextAction]

    def divideParticles(self, currentTotalLayer, fitness):
        # 将粒子（self.pN 个粒子）按照它们的适应度（fitness）分配到不同的层级（currentTotalLayer 层），
        # 使得每一层的粒子数量尽可能均衡
        baseCount = self.pN // currentTotalLayer
        remainder = self.pN % currentTotalLayer
        layerCounts = [baseCount] * (currentTotalLayer - 1) + [
            baseCount + remainder
        ]

        particles = list(range(0, self.pN))
        sortedParticles = [x for _, x in sorted(zip(fitness, particles))]

        self.layers.clear()
        start_idx = 0
        for count in layerCounts:
            end_idx = start_idx + count
            self.layers.append(sortedParticles[start_idx:end_idx])
            start_idx = end_idx

    def levelCompetition(self, lec, FECount):
        # 在进行某种竞争或选择时，基于给定的条件随机选择和比较两个“层次”（levels），并返回一个排序后的结果
        prob = (FECount / self.max_iter)**2
        exemplarLevels = [None, None]
        for i in range(2):
            if random.random() < prob:
                lec1 = random.randint(0, lec - 1)
                lec2 = random.randint(0, lec - 1)
                if lec1 < lec2:
                    exemplarLevels[i] = lec1
                else:
                    exemplarLevels[i] = lec2
            else:
                exemplarLevels[i] = random.randint(0, lec - 1)

        if exemplarLevels[1] < exemplarLevels[0]:
            exemplarLevels[0], exemplarLevels[1] = exemplarLevels[
                1], exemplarLevels[0]
        return exemplarLevels

    def init_population(self):
        # 初始化粒子群的位置信息和速度信息，前半部分粒子使用对数均匀分布，而后半部分粒子使用范围 [-10, 10] 的均匀分布进行初始化。。
        # 并初始化局部和全局最优位置信息
        for i in range(self.pN):
            if (i <= self.pN / 2):
                self.X[i] = np.exp(
                    np.log(self.lower) + np.log(self.upper / self.lower) *
                    np.random.uniform(0, 1, self.dim)) * np.random.choice(
                        [-1, 1], self.dim)
                self.V[i] = np.exp(
                    np.log(self.lower) + np.log(self.upper / self.lower) *
                    np.random.uniform(0, 1, self.dim)) * np.random.choice(
                        [-1, 1], self.dim)
            else:
                self.X[i] = [random.uniform(-10, 10) for _ in range(self.dim)]
                self.V[i] = [random.uniform(-10, 10) for _ in range(self.dim)]
            self.pBest[i] = self.X[i]
            tmp = self.function(self.X[i])
            self.fitness.append(tmp)
            self.p_fit[i] = tmp
            if tmp < self.fit:
                self.fit = tmp
                self.gBest = self.X[i]

    def random_small_value(self):
        lower_bound = -0.1
        upper_bound = 0.1
        return random.uniform(lower_bound, upper_bound)

    def iterator(self):
        for t in range(self.max_iter):
            pregBest = self.gBest
            currentTotalLayer = self.selectAction()

            self.divideParticles(currentTotalLayer, self.fitness)
            for i in range(currentTotalLayer - 1, 1, -1):  # 从最高层向下遍历到第2层
                for j in self.layers[i]:  # 遍历当前层的所有粒子
                    exemplarLevels = self.levelCompetition(
                        i, t)  # exemplarLevels有两层，都是[0, i - 1]之间的整数
                    if (exemplarLevels[0] == exemplarLevels[1]):
                        index1 = random.randint(
                            0,
                            len(self.layers[exemplarLevels[0]]) - 2)
                        index2 = random.randint(
                            index1 + 1,
                            len(self.layers[exemplarLevels[0]]) - 1)
                        id1 = self.layers[exemplarLevels[0]][index1]
                        id2 = self.layers[exemplarLevels[0]][index2]
                    else:
                        id1 = random.choice(self.layers[exemplarLevels[0]])
                        id2 = random.choice(self.layers[exemplarLevels[1]])
                    X1 = self.X[id1]
                    X2 = self.X[id2]

                    self.r1 = random.uniform(0, 1)
                    self.r2 = random.uniform(0, 1)
                    self.r3 = random.uniform(0, 1)

                    self.V[j] = self.r1 * self.V[j] + self.r2 * (
                        X1 - self.X[j]) + self.r3 * self.phi * (X2 - self.X[j])
                    self.X[j] = self.X[j] + self.V[j]

            for k in self.layers[1]:

                index1 = random.randint(0, len(self.layers[0]) - 2)
                index2 = random.randint(index1 + 1, len(self.layers[0]) - 1)
                id1 = self.layers[0][index1]
                id2 = self.layers[0][index2]
                X1 = self.X[id1]
                X2 = self.X[id2]

                self.r1 = random.uniform(0, 1)
                self.r2 = random.uniform(0, 1)
                self.r3 = random.uniform(0, 1)

                self.V[k] = self.r1 * self.V[k] + self.r2 * (
                    X1 - self.X[k]) + self.r3 * self.phi * (X2 - self.X[k])

                self.X[k] = self.X[k] + self.V[k]

            if (t == int(self.max_iter / 2) and self.fit > 1e-04):
                # 在迭代到一半（t == self.max_iter / 2）且当前最优适应值 self.fit 仍然较大时（self.fit > 1e-04），对所有粒子的位置 (self.X[i]) 和速度 (self.V[i]) 进行重新初始化。
                for i in range(self.pN):
                    # for j in range(self.dim):
                    self.X[i] = np.exp(
                        np.log(self.lower) + np.log(self.upper / self.lower) *
                        np.random.uniform(0, 1, self.dim)) * np.random.choice(
                            [-1, 1], self.dim)
                    self.V[i] = np.exp(
                        np.log(self.lower) + np.log(self.upper / self.lower) *
                        np.random.uniform(0, 1, self.dim)) * np.random.choice(
                            [-1, 1], self.dim)

            self.fitness.clear()
            for i in range(self.pN):
                temp = self.function(self.X[i])
                self.fitness.append(temp)
                if temp < self.p_fit[i]:
                    self.p_fit[i] = temp
                    self.pBest[i] = self.X[i]
                    if self.p_fit[i] < self.fit:
                        self.gBest = self.X[i]
                        self.fit = self.p_fit[i]

            preFitness = self.function(pregBest)
            curFitness = self.function(self.gBest)
            reward = abs(curFitness - preFitness) / abs(max(curFitness, 1e-10))
            newQ = (
                self.qTable[self.preState][self.currentState] + self.alpha *
                (reward + self.gamma * max(self.qTable[self.currentState]) -
                 self.qTable[self.preState][self.currentState]))
            self.qTable[self.preState][self.currentState] = newQ

            print(self.fit)

    def get_gBest(self):
        return self.gBest

    def get_fit(self):
        return self.fit

    def get_iter(self):
        return self.iteratorMse


data = []

with open('lys-bh.CSV.csv', 'r') as csvfile:
    csvreader = csv.reader(csvfile)
    next(csvreader)
    for row in csvreader:
        row = [float(value) for value in row]
        data.append(row)

my_pso = PSO(pN=100,
             dim=5,
             max_iter=1000,
             numberOfLayers_list=[4, 6, 8, 10],
             actual_data=data)
random.seed(123)
np.random.seed(123)
my_pso.init_population()
my_pso.iterator()
print(my_pso.get_gBest())
