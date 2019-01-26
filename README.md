1: Equilibrium concentrations. This section of KEV software is intended to calculate the equilibrium composition of the chemical mixture. The reactions occurring in the mixture as well as their decimal logarithms of equilibrium constants and total (equilibrium) concentrations of the reagents should be set as input data in order to obtain equilibrium concentrations of reagents and products and products yields in relation to any chosen reagent.

2: Equilibrium constants via UV-Vis spectroscopy. This section of KEV software is intended to optimize the unknown equilibrium constants values using UV-Vis spectroscopy data. The reactions occurring in the mixture, their decimal logarithms of equilibrium constants, total concentrations of the reagents, experimental absorbance values at different wavelengths as well as known molar extinction coefficients should be set as input data. The name(s) of reaction(s) which constants are to be optimized should also be specified. KEV software returns equilibrium concentrations of reagents and products, calculated absorbances with absolute/relative deviations from experimental values, optimized constants values, calculated molar extinction coefficients, and correlation matrix.

3: Equilibrium constants via potentiometric measurements. This section of KEV software is intended to optimize the unknown equilibrium constants values using potentiometric titration/measurements data. The reactions occurring in the mixture, their decimal logarithms of equilibrium constants, total concentrations of the reagents, experimental potential differences between indicator and reference electrods as well as standard potential and Nernst slope should be set as input data. The name(s) of reaction(s) which constants are to be optimized should also be specified. KEV software returns equilibrium concentrations of reagents and products and products, calculated EMF with absolute/relative deviations from experimental values, optimized constants values, and correlation matrix.

Some more complex (or less stable) systems require higher numeric precision not available in the plain R. KEV is tested with Microsoft Open R (MRO) + Intel Math Kernel Library (MKL) and works fine with it. You can also try to build R with MKL but it is not yet tested. It may be OK also to use other math and specfically matrix computation libraries such as OpenBLAS but it is not tested as well.

[https://k-ev.org/kev](https://k-ev.org/kev) KEV web application. Use it if you do not feel comfortable with scripts and terminals.