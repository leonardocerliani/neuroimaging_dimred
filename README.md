## Interactive atlas-based embedding of neuroimaging results

[Interact live with this app on shinyapps.io](https://agronomous.shinyapps.io/neuroimaging_dimred/) \
(free account, so please allow a few seconds to load).

This app allows to explore which combination of regions from an atlas yields the maximum separation between tasks/conditions/groups. The latter is estimated using different types of dimensionality reduction techniques.

The low-dimensional embedding can then be inspected to see the profiles of the single maps which were grouped together.

![image](neuroimaging_dimred.gif)


While it was tested here with (synthetic) fMRI data, it can be applied to any kind of modality which produces a result in a template space. 

In a further development, several atlases will be available, as well as tuning parameters for different dimensionality reduction methods.
