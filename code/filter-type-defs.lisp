;;; -*- Mode: Common-lisp; Package: Natsoft; readtable: Joshua -*-

(in-package :natsoft)



(defdata-type ADAPTIVE-CONTRAST-ENHANCEMENT :super-types (image-processing-filter))

(defdata-type ADD :super-types (image-processing-filter))

(defdata-type ADDITIVE-NOISE :super-types (image-processing-filter))

(defdata-type ALPHA-TRIMMED-MEAN :super-types (image-processing-filter))

(defdata-type AND :super-types (image-processing-filter))

(defdata-type ARTIFACTS-REMOVAL :super-types (image-processing-filter))

(defdata-type BERNSEN-THRESHOLD :super-types (image-processing-filter))

(defdata-type BINARY-CLOSING :super-types (image-processing-filter))

(defdata-type BINARY-DILATATION :super-types (image-processing-filter))

(defdata-type BINARY-EROSION :super-types (image-processing-filter))

(defdata-type BINARY-OPENING :super-types (image-processing-filter))

(defdata-type BINARY-WATERSHED :super-types (image-processing-filter))

(defdata-type BLOBS-FILTERING :super-types (image-processing-filter))

(defdata-type BLUR :super-types (image-processing-filter))

(defdata-type BOTTOM-HAT :super-types (image-processing-filter))

(defdata-type BRADLEY-LOCAL-THRESHOLD :super-types (image-processing-filter))

(defdata-type BRIGHTNESS-CORRECTION :super-types (image-processing-filter))

(defdata-type CANNY-EDGE-DETECTOR :super-types (image-processing-filter))

(defdata-type CLAHE :super-types (image-processing-filter))

(defdata-type CLOSING :super-types (image-processing-filter))

(defdata-type COLOR-FILTERING :super-types (image-processing-filter))

(defdata-type CONSERVATIVE-SMOOTHING :super-types (image-processing-filter))

(defdata-type CONTRAST-CORRECTION :super-types (image-processing-filter))

(defdata-type CONVOLUTION :super-types (image-processing-filter))

(defdata-type COSINE-TRANSFORM :super-types (image-processing-filter))

(defdata-type CROP :super-types (image-processing-filter))

(defdata-type DESATURATION :super-types (image-processing-filter))

(defdata-type DIFFERENCE :super-types (image-processing-filter))

(defdata-type DIFFERENCE-EDGE-DETECTOR :super-types (image-processing-filter))

(defdata-type DILATATION :super-types (image-processing-filter))

(defdata-type DISPARITY-MAP :super-types (image-processing-filter))

(defdata-type DISTANCE-TRANSFORM :super-types (image-processing-filter))

(defdata-type DIVISION :super-types (image-processing-filter))

(defdata-type EMBOSS :super-types (image-processing-filter))

(defdata-type ENSEMBLE-THRESHOLD :super-types (image-processing-filter))

(defdata-type EROSION :super-types (image-processing-filter))

(defdata-type EXP :super-types (image-processing-filter))

(defdata-type EXTRACT-BOUNDARY :super-types (image-processing-filter))

(defdata-type EXTRACT-NORMALIZED-RGBCHANNEL :super-types (image-processing-filter))

(defdata-type EXTRACT-RGBCHANNEL :super-types (image-processing-filter))

(defdata-type EXTRACT-YCB-CR-CHANNEL :super-types (image-processing-filter))

(defdata-type FAST-VARIANCE :super-types (image-processing-filter))

(defdata-type FILL-HOLES :super-types (image-processing-filter))

(defdata-type FLOOD-FILL :super-types (image-processing-filter))

(defdata-type FOURIER-TRANSFORM :super-types (image-processing-filter))

(defdata-type FREQUENCY-FILTER :super-types (image-processing-filter))

(defdata-type GABOR-FILTER :super-types (image-processing-filter))

(defdata-type GAMMA-CORRECTION :super-types (image-processing-filter))

(defdata-type GAUSSIAN-BLUR :super-types (image-processing-filter))

(defdata-type GAUSSIAN-BOX-BLUR :super-types (image-processing-filter))

(defdata-type GAUSSIAN-NOISE :super-types (image-processing-filter))

(defdata-type GRADIENT-MAP :super-types (image-processing-filter))

(defdata-type GRAYSCALE :super-types (image-processing-filter))

(defdata-type GRAYSCALE-TO-RGB :super-types (image-processing-filter))

(defdata-type GRAY-WORLD :super-types (image-processing-filter))

(defdata-type HIGH-BOOST :super-types (image-processing-filter))

(defdata-type HISTOGRAM-ADJUST :super-types (image-processing-filter))

(defdata-type HISTOGRAM-EQUALIZATION :super-types (image-processing-filter))

(defdata-type HISTOGRAM-MATCHING :super-types (image-processing-filter))

(defdata-type HISTOGRAM-SHRINK :super-types (image-processing-filter))

(defdata-type HISTOGRAM-STRETCH :super-types (image-processing-filter))

(defdata-type HOMOGENITY-EDGE-DETECTOR :super-types (image-processing-filter))

(defdata-type HOMOMORPHIC-FILTER :super-types (image-processing-filter))

(defdata-type HORIZONTAL-INTENSITY-STATISTICS :super-types (image-processing-filter))

(defdata-type HORIZONTAL-RUN-LENGTH-SMOOTHING :super-types (image-processing-filter))

(defdata-type HSLFILTERING :super-types (image-processing-filter))

(defdata-type HSLLINEAR :super-types (image-processing-filter))

(defdata-type HUE-MODIFIER :super-types (image-processing-filter))

(defdata-type HYSTERESIS-THRESHOLD :super-types (image-processing-filter))

(defdata-type IMAGE-NORMALIZATION :super-types (image-processing-filter))

(defdata-type IMAGE-PYRAMIDS :super-types (image-processing-filter))

(defdata-type IMAGE-QUANTIZATION :super-types (image-processing-filter))

(defdata-type INTERSECT :super-types (image-processing-filter))

(defdata-type INVERT :super-types (image-processing-filter))

(defdata-type ISO-DATA-CLASSIFIER :super-types (image-processing-filter))

(defdata-type ISOTROPIC-COMPASS-EDGE-DETECTOR :super-types (image-processing-filter))

(defdata-type KIRSCH-COMPASS-EDGE-DETECTOR :super-types (image-processing-filter))

(defdata-type KUWAHARA :super-types (image-processing-filter))

(defdata-type LEVELS-CURVE :super-types (image-processing-filter))

(defdata-type LEVELS-LINEAR :super-types (image-processing-filter))

(defdata-type LOG :super-types (image-processing-filter))

(defdata-type MAXIMUM :super-types (image-processing-filter))

(defdata-type MAXIMUM-ENTROPY-THRESHOLD :super-types (image-processing-filter))

(defdata-type MEAN :super-types (image-processing-filter))

(defdata-type MEAN-SHIFT :super-types (image-processing-filter))

(defdata-type MEDIAN :super-types (image-processing-filter))

(defdata-type MEDIAN-CUT :super-types (image-processing-filter))

(defdata-type MERGE :super-types (image-processing-filter))

(defdata-type MID-POINT :super-types (image-processing-filter))

(defdata-type MINIMUM :super-types (image-processing-filter))

(defdata-type MIRROR :super-types (image-processing-filter))

(defdata-type MODE :super-types (image-processing-filter))

(defdata-type MODIFIED-WHITE-PATCH :super-types (image-processing-filter))

(defdata-type MORPH :super-types (image-processing-filter))

(defdata-type MORPHOLOGIC-GRADIENT-IMAGE :super-types (image-processing-filter))

(defdata-type MOVE-TOWARDS :super-types (image-processing-filter))

(defdata-type MULTIPLY :super-types (image-processing-filter))

(defdata-type NAND :super-types (image-processing-filter))

(defdata-type NIBLACK-THRESHOLD :super-types (image-processing-filter))

(defdata-type NICK-THRESHOLD :super-types (image-processing-filter))

(defdata-type NOR :super-types (image-processing-filter))

(defdata-type OPENING :super-types (image-processing-filter))

(defdata-type OR :super-types (image-processing-filter))

(defdata-type OTSU-THRESHOLD :super-types (image-processing-filter))

(defdata-type OUTLINE :super-types (image-processing-filter))

(defdata-type PERONA-MALIK-ANISOTROPIC-DIFFUSION :super-types (image-processing-filter))

(defdata-type POISSON-NOISE :super-types (image-processing-filter))

(defdata-type PREWITT-COMPASS-EDGE-DETECTOR :super-types (image-processing-filter))

(defdata-type PRINCIPAL-COMPONENT-TRANSFORM :super-types (image-processing-filter))

(defdata-type RANDOM-CONVOLUTION :super-types (image-processing-filter))

(defdata-type REPLACE-COLOR :super-types (image-processing-filter))

(defdata-type REPLACE-RGBCHANNEL :super-types (image-processing-filter))

(defdata-type RESIZE :super-types (image-processing-filter))

(defdata-type RESIZE-BICUBIC :super-types (image-processing-filter))

(defdata-type RESIZE-BILINEAR :super-types (image-processing-filter))

(defdata-type RESIZE-NEAREST-NEIGHBOR :super-types (image-processing-filter))

(defdata-type RGCHROMATICITY :super-types (image-processing-filter))

(defdata-type ROBERTS-CROSS-EDGE-DETECTOR :super-types (image-processing-filter))

(defdata-type ROBINSON-COMPASS-EDGE-DETECTOR :super-types (image-processing-filter))

(defdata-type ROSIN-THRESHOLD :super-types (image-processing-filter))

(defdata-type ROTATE :super-types (image-processing-filter))

(defdata-type ROTATE-BICUBIC :super-types (image-processing-filter))

(defdata-type ROTATE-BILINEAR :super-types (image-processing-filter))

(defdata-type ROTATE-CHANNELS :super-types (image-processing-filter))

(defdata-type ROTATE-NEAREST-NEIGHBOR :super-types (image-processing-filter))

(defdata-type SALT-AND-PEPPER-NOISE :super-types (image-processing-filter))

(defdata-type SATURATION-CORRECTION :super-types (image-processing-filter))

(defdata-type SAUVOLA-THRESHOLD :super-types (image-processing-filter))

(defdata-type SCHARR-COMPASS-EDGE-DETECTOR :super-types (image-processing-filter))

(defdata-type SEPARABLE-CONVOLUTION :super-types (image-processing-filter))

(defdata-type SEPIA :super-types (image-processing-filter))

(defdata-type SHARPEN :super-types (image-processing-filter))

(defdata-type SHRINK :super-types (image-processing-filter))

(defdata-type SISTHRESHOLD :super-types (image-processing-filter))

(defdata-type SOBEL-COMPASS-EDGE-DETECTOR :super-types (image-processing-filter))

(defdata-type SOBEL-EDGE-DETECTOR :super-types (image-processing-filter))

(defdata-type STEREO-ANAGLYPH :super-types (image-processing-filter))

(defdata-type SUBTRACT :super-types (image-processing-filter))

(defdata-type THRESHOLD :super-types (image-processing-filter))

(defdata-type TOP-HAT :super-types (image-processing-filter))

(defdata-type TSAI-THRESHOLD :super-types (image-processing-filter))

(defdata-type UNSHARP-MASKING :super-types (image-processing-filter))

(defdata-type VARIANCE :super-types (image-processing-filter))

(defdata-type VERTICAL-INTENSITY-STATISTICS :super-types (image-processing-filter))

(defdata-type VERTICAL-RUN-LENGTH-SMOOTHING :super-types (image-processing-filter))

(defdata-type WAVELET-TRANSFORM :super-types (image-processing-filter))

(defdata-type WEIGHTED-MEDIAN :super-types (image-processing-filter))

(defdata-type WHITE-PATCH :super-types (image-processing-filter))

(defdata-type WOLF-JOLION-THRESHOLD :super-types (image-processing-filter))

(defdata-type XOR :super-types (image-processing-filter))

(defdata-type YCB-CR-FILTERING :super-types (image-processing-filter))

(defdata-type ZHANG-SUEN-THINNING :super-types (image-processing-filter))