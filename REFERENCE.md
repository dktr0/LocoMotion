
# LocoMotion Reference

## Introduction

As mentioned in the [LocoMotion README](https://github.com/dktr0/LocoMotion/blob/main/README.md#getting-started), the LocoMotion demo is accessible through both the Estuary platform and the demo program in the standalone at https://dktr0.github.io/LocoMotion. The following are the instructions for getting started with LocoMotion:

1. Pick a model as your dancer.
2. Change the size or relocate the dancer in the provided environment.
3. Adjust the floor color or the provided globe environment.
4. Relocate the camera in the provided environment.

<br>
</br>

## Setup and Usage 

LocoMotion does not require any installation, as it can be accessed directly through a web browser or Estuary. There are two primary ways to use LocoMotion:

### 1. Stanalone Demo

You can start experimenting with LocoMotion by visiting the standalone demo at https://dktr0.github.io/LocoMotion. This demo allows you to enter LocoMotion code, evaluate it, and see the results in real-time.

### 2. Estuary Platform

LocoMotion can also be used within the Estuary collaborative live coding platform at https://estuary.mcmaster.ca. To use LocoMotion in Estuary, follow these steps:

> 
> 1. Visit https://estuary.mcmaster.ca in your web browser.
> 2. Create a new code panel by clicking the "+" button at the bottom of the screen.
> 3. In the newly created code panel, do not select any language from the drop-down menu.
> 4. To access LocoMotion, type ``##locomotion`` in the first line of the code panel.
> 5. Start entering your LocoMotion code in the panel and press Shift+Enter to evaluate it.

<br>
</br>

## Dancers and Models

To create and manipulate dancers in LocoMotion, you need to define a dancer block and provide the necessary parameters. Below is a brief overview and example of how to work with dancers and models:

## Dancer Block

A dancer block allows you to specify the dancer's model, size, and position in the 3D space. Here's a basic example:

```
dancer { url="lisa.glb", size = 0.6, x = 1, y = 0, z = 0 };
```
You can create multiple dancers by separating each dancer block with a semicolon ``;`` ``:`` 

```
dancer { url="lisa.glb", x = 1, y = 0, z = 0 };
dancer { url="alan.glb", x = -1, y = 0, z = 0 };
```



All properties that can be set on dancer elements:
<br>

- url : You can change the dancer model by inputting a new `url`. Find the current models available at the following link: https://github.com/dktr0/LocoMotion/tree/main/models.
<br>
- Position: Which is shown with ``x``, ``y``, ``z``
<br>

- Rotation: Rotation allows you to change the orientation of a dancer model in LocoMotion. There are three rotation axes you can modify:
>  - ``rx``: Rotation around the X axis
>  - ``ry``: Rotation around the Y axis
>  - ``rz``: Rotation around the Z axis

Rotation values are in degrees, with a range of 0 to 360. A 360-degree rotation brings the model back to its original orientation.
You can also combine multiple rotations to achieve more complex orientations.
<br>

- Look at: The "look at" feature in LocoMotion enables a dancer model to face a specific point in the 3D space. To use this feature, you need to provide the coordinates of the target point using the following parameters:

>  - ``lx``: Look at around the X axis
>  - ``ly``: Look at around the Y axis
>  - ``lz``: Look at around the Z axis

Note that the dancer's position may affect the result of the "look at" feature, so adjusting the dancer's position (x, y, and z) may be necessary to achieve the desired effect.
<br>

- Size: Which is shown with ``sx``, ``sy``, ``sz``
<br>

- Animation: Each dancer model in LocoMotion has multiple animations associated with it. You can change the ``animation`` by specifying the animation parameter with a number corresponding to the desired animation.Also, that different models might have different animation sets, so some animations may not look the same or even work as expected when applied to other models.
<br>

- Duration: 
The duration parameter (``dur``) in LocoMotion controls the pace of the dancer's movement. A smaller value for ``dur`` results in a faster pace, while a larger value slows down the movement. Keep in mind that ``dur `` values should be positive rational numbers (Q). Using a negative or zero value for ``dur`` may result in unexpected behavior or errors.
<br>

- Oscillation: 
Oscillation in LocoMotion enables you to create smooth, periodic movements for dancer models. You can use oscillators to modify the position of dancers along the X, Y, and Z axes. Oscillators output values between -1 and 1 by default.

To use oscillation, apply the osc function with a frequency value:
```
x = osc FREQUENCY
```
<br>

</br>

> ### PS:
> Remember that the numbers entered should be Rational numbers (Q). For example, if you want to log "Lisa" as your dancer, the following could be the required code for evaluating in the LocoMotion:
>```
>dancer { url="lisa.glb", size = 0.0 , x = -3 };
>```
>

<br>
</br>

 ### Models

To use a model, specify its name (case-sensitive) in the ``url`` parameter of the dancer block:

```
dancer { url="NatureGirl.glb", x = 0, y = 0, z = 0 };
```

<br>
</br>

## Camera 


ocoMotion allows you to reposition the camera in the 3D space to view the scene from different angles. To adjust the camera position, use the camera function with the following parameters:

- `x`: Camera's X axis position
- `y`: Camera's Y axis position
- `z`: Camera's Z axis position

Note that when using LocoMotion in Estuary (https://estuary.mcmaster.ca), you must place the camera code in the lower right-hand corner box for it to function properly. There can be only one camera panel.

<br>
</br>

## Lightning


LocoMotion provides various lighting options to illuminate the scene, including ambient, point, and directional lights. Each light type can accept an intensity parameter to control the brightness of the light.

<br>
</br>

### Ambient Light
<br>


> ``ambient { intensity = ANUMBER }``
>


For example, to create a dynamic ambient light that oscillates between 0.3 and 1.0 intensity:

>`` ambient { intensity = range 0.3 1.0 (osc 0.5) }
``
>

<br>
</br>

### Point Light 

Point light simulates a light bulb, emitting light in all directions from a single point. To create a point light, use the point function with the following parameters:

- `x`: Light's X axis position
- `y`: Light's Y axis position
- `z`: Light's Z axis position
color: Light's color as a hexadecimal value

For example, to create a green point light at position (0, 2, 0):
> ``` point { x = 0, y = 2, z = 0, color = 0x00FF00 }```

<br>
</br>

### Directional Light

Directional light emits light in a specific direction, as if it were coming from a distant source like the sun. To create a directional light, use the directional function with the following parameters:
- `x`: Light's X axis position
- `y`: Light's Y axis position
- `z`: Light's Z axis position

For example, to create a directional light at position (10, 0, 0):

>```directional { x = 10 }```

Feel free to experiment with different light types, positions, and intensities to create the desired atmosphere for your LocoMotion scene.

<br>
</br>

## Intensity & Oscillation

Each light can accept an `intensity` parameter to indicate how bright the light is. Point and Directional light can be given a position. A point light emits in all directions whereas directional emits in a specific direction.
For example:

>```directional { x = 0, y = 0, z = 5, intensity = 1 }```

You can use the oscillator functions on the lighting parameters as well:

> ```ambient {intensity = 0.1 };```
```directional { x = 0, y = 0, z = 5, intensity = range 0 1 (osc 1) } ```
>
</br>

## Movement and Choreography

As mentioned in the dancers and models sections, 
In the REFERENCE.md file, you can include sections about movement and choreography as follows:

Movement
LocoMotion enables users to create custom movements for their characters by applying various functions and combining them as needed. To create movement, apply the following functions to the relevant body parts:

- `rotate`: Rotates a body part around a specified axis (X, Y, or Z) by a given angle.
- `osc` : Creates oscillating movements by specifying the range, frequency, and phase shift.

Choreography in LocoMotion involves creating a sequence of movements and poses for characters to perform over time. By synchronizing body movements and adjusting their timing, you can create complex and dynamic performances.

<br>
</br>


## Creating Planes

To create a plane, for example to create a floor, use the `plane` object. You can use the parameter `sx` `sy` `sz` to change the size in each dimension

` plane { rx = 270, sx = 20 } `

Combine multiple elements with a plane and a dancer:

``` dancer { url = "lisa.glb", dur = 3, x = range (-2) 2 (osc 0.25), y = (-1), z = 1, lx = 0, ly = 0, lz = 0 };```

``` plane { rx = 270, sx = 20, sy = 200, z = 0, y = -2 }```

<br>

>You can change the color of objects using hexadecimal color codes. You can find hexadecimal color codes to use on the web at sites like this: https://www.w3schools.com/colors/colors_picker.asp Make sure you prepend each 6 digit color code with `0x` otherwise LocoMotion won't understand.
>
>
> ``` dancer { url = "lisa.glb", dur = 3, x = range (-2) 2 (osc 0.25), y = (-1), z = 1, lx = 0, ly = 0, lz = 0 };```
>
>```plane { rx = 270, sx = 20, sy = 200, z = 0, y = -2, color = 0xaa32ff }```
> 
<br>
</br>


## License and Atrribution
LocoMotion is free-and-open-source software, released under the terms of the GNU Public License, version 3.

<br>
</br>


# 