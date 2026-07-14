# 🌲 Rforestat

> 📘 《林业统计建模与 R 语言》配套 R 软件包  
> 📘 Companion R package for the book *"Forest Statistics Modeling with R"*

---

## 📦 软件包简介 | Package Overview

`forestat` 是为《林业统计建模与 R 语言》一书配套开发的 R 语言软件包，内含完整的示例代码、函数与数据集，旨在帮助读者系统学习林业统计模型与 R 应用。  

The `forestat` package accompanies the book *Forest Statistics Modeling with R*. It contains all example codes, functions, and datasets used in the book, enabling readers to practice and learn forest statistics modeling using R.

---

## 📂 数据集简介 | Included Datasets

该软件包包含以下林业与遥感相关数据集：

 This package includes the following forest-related and remote sensing datasets:

- `data(larch)`：冬奥核心区 **华北落叶松** 数据集
   *Larch dataset from the Winter Olympics core area*
- `data(birch)`：冬奥核心区 **白桦** 数据集
   *Birch dataset from the Winter Olympics core area*
- `data(picea)`：**雷达反演** 数据集
   *Radar-derived dataset for forest structure*
- `data(forestData)`：**森林碳汇** 数据集
   *Forest carbon sink estimation dataset*

---

## 🔧 安装方法 | Installation

请使用 `devtools` 安装本地打包文件：  
Use `devtools` to install from the local `.tar.gz` package file:

```r
install.packages("devtools")  # 如尚未安装 devtools
devtools::install.packages("forestat_1.2.0.tar.gz", repos = NULL, type = "source")
```

------

## 📘 教材配套说明 | Book Integration

本包为《林业统计建模与 R 语言》中所有章节的**案例和代码实现提供支持**，适合教学、科研与工程实践使用。

This package fully supports the examples and code in *Forest Statistics Modeling with R*, and is suitable for use in education, research, and applied forest analytics.

------

## 📖 参考文献 | Reference

> 作者：XXX，《林业统计建模与 R 语言》，出版年份：20XX
>
> *Author: XXX, "Forest Statistics Modeling with R", Published: 20XX*

