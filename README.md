# KEV: Constant Evaluator

[KEV: Constant Evaluator](https://k-ev.org) is a chemical software developed for solving two classes of problems of the chemical equilibria theory. It is free, open source and available both as an online service and scripts bundle.

### 1. Equilibrium concentrations

Calculating the equilibrium composition of the chemical mixture. The reactions occurring in the mixture as well as their decimal logarithms of equilibrium constants and total (equilibrium) concentrations of the reagents should be set as input data in order to obtain equilibrium concentrations of reagents and products and products yields in relation to any chosen reagent.

### 2. Equilibrium constants

#### 2.1. Equilibrium constants via UV-Vis spectroscopy

Optimize the unknown equilibrium constants values using UV-Vis spectroscopy data. The reactions occurring in the mixture, their decimal logarithms of equilibrium constants, total concentrations of the reagents, experimental absorbance values at different wavelengths as well as known molar extinction coefficients should be set as input data. The name(s) of reaction(s) which constants are to be optimized should also be specified. KEV software returns equilibrium concentrations of reagents and products, calculated absorbances with absolute/relative deviations from experimental values, optimized constants values, calculated molar extinction coefficients, correlation matrix, and adjusted determination coefficient indicating the proximity between experimental and calculated data.

#### 2.2. Equilibrium constants via potentiometric measurements

Optimizing the unknown equilibrium constants values using potentiometric titration/measurements data. The reactions occurring in the mixture, their decimal logarithms of equilibrium constants, total concentrations of the reagents, experimental potential differences between indicator and reference electrods as well as standard potential and Nernst slope should be set as input data. The name(s) of reaction(s) which constants are to be optimized should also be specified. KEV software returns equilibrium concentrations of reagents and products and products, calculated EMF with absolute/relative deviations from experimental values, optimized constants values, correlation matrix, and adjusted determination coefficient indicating the proximity between experimental and calculated data.

#### 2.3. Equilibrium constants via NMR spectroscopy (fast exchange)

Optimizing the unknown equilibrium constants values using NMR spectroscopy data. The reactions occurring in the mixture, their decimal logarithms of equilibrium constants, total concentrations of the reagents, experimental chemical shifts of different nuclei of indicator component mixture with its derivatives, as well as chemical shifts of nuclei of individual species (if known) should be set as input data. The name(s) of reaction(s) which constants are to be optimized should also be specified. KEV software returns equilibrium concentrations of reagents and products, calculated chemical shifts with absolute/relative deviations from experimental values, optimized constants values, calculated chemical shifts for individula species, correlation matrix, and adjusted determination coefficient indicating the proximity between experimental and calculated data.

#### 2.4. Equilibrium constants via NMR Calorimetry

Optimizing the unknown equilibrium constants values via calorimetry data.

### 3. Curve fitting

Fitting the experimental curve, e.g. some absorption spectra (IR or UV-Vis), with bunch of lorenz or gauss functions. The experimental curve is uploaded as table "wavelength(wavenumber)-intensity", and KEV suggests some functions to approximate the input data. These suggestions could be either deleted or modified; or additional functions could be added. KEV optimizes the key parameters of lorentz or gauss functions used for describinf the experimental curve, namely, amplitude, x-axis position and HWHM and returnes them as well as parameters characterizing the quality of fitting procedure (adjusted determination coefficient, RMSE, MAE).    

### Environment

Some more complex (or less stable) systems require higher numeric precision not available in the plain R. KEV is tested with Microsoft Open R (MRO) + Intel Math Kernel Library (MKL) and works fine with it. You can also try to build R with MKL but it is not yet tested. It may be OK also to use other math and specfically matrix computation libraries such as OpenBLAS but it is not tested as well.

[https://k-ev.org](https://k-ev.org) KEV web application. Use it if you do not feel comfortable with scripts and terminals.

### Licensing

**KEV License**. As free and open source software KEV is licensed under GNU GPL 3.0 license, see the full text of the license in the LICENSE file in the root folder of the project.

**Data Licenses**. KEV includes some example data to facilitate getting started with KEV. Specific licences for every data set are included in *input* folder subfolders.
