# Point sampling class
The system `point-sampling` implements a class to represent samplings of `point`
elements.
It is built on top of the [point](https://github.com/thomashoullier/point)
class.

## Scope
`point-sampling` implements only the class and basic methods.
The generation of point sampling schemes (eg. grids, uniform random etc.) is
outside of the scope.

## Usage
`point-sampling` instances hold a vector of `point` elements.

### Instantiators
Several instantiators are implemented for convenience.

**make-point-sampling-empty** => point-sampling

Create an empty point-sampling instance, to be filled with points later.

**make-point-sampling-frompoints** points-vec => point-sampling

Create a point-sampling instance from a vector of `point` elements.

**make-point-sampling-fromtable** points-table => point-sampling

Create a point-sampling instance from a vector of coordinates. This
allows initializing point-sampling from native vectors.

```common-lisp
(make-point-sampling-empty)
;=> #<POINT-SAMPLING #()>

(defparameter *p1* (make-point 1 2))
(defparameter *p2* (make-point 3 4))
(defparameter *points-vec* (make-array 2 :initial-contents (list *p1* *p2*)))
(make-point-sampling-frompoints *points-vec*)
;=> #<POINT-SAMPLING #(#<POINT #(1 2)> #<POINT #(3 4)>)>

(make-point-sampling-fromtable #(#(1 2) #(3 4)))
;=> #<POINT-SAMPLING #(#<POINT #(1 2)> #<POINT #(3 4)>)>
```

### Basic methods
**ps-n** point-sampling => n

Return the number of points in the point-sampling.

**ps-deepcpy** point-sampling => point-sampling-copy

Return a deep copy (with all points copied) of a point-sampling.

### Point push, pop and access
**ps-push** new-point point-sampling => new-index-p

**ps-pop** new-point point-sampling => point

`vector-push-extend` and `vector-pop` the vector of points.

**ps-ref** point-sampling index => point

Access a point element in point-sampling by its index.

**points** point-sampling => points-vector

Direct access to the underlying points vector of the point-sampling.

```common-lisp
(defparameter *ps* (make-point-sampling-fromtable #(#(1 2) #(3 4))))
*ps* ;=> #<POINT-SAMPLING #(#<POINT #(1 2)> #<POINT #(3 4)>)>

(ps-push (make-point 5 6) *ps*)
*ps* ;=> #<POINT-SAMPLING #(#<POINT #(1 2)> #<POINT #(3 4)> #<POINT #(5 6)>)>

(ps-pop *ps*)
;=> #<POINT #(5 6)>

(ps-ref *ps* 1)
;=> #<POINT #(3 4)>

(setf (ps-ref *ps* 1) (make-point 10 11))
*ps* ;=> #<POINT-SAMPLING #(#<POINT #(1 2)> #<POINT #(10 11)>)>

(setf (px (ps-ref *ps* 1)) 15)
*ps* ;=> #<POINT-SAMPLING #(#<POINT #(1 2)> #<POINT #(15 11)>)>
```

### Equality predicate
Two point-sampling instances are equal if they contain the same points,
including the same number of eventual duplicates. Note this is not
an usual set equality since duplicates are included.

**ps-eq** ps1 ps2 => boolean

```common-lisp
(defparameter *ps1* (make-point-sampling-fromtable #(#(1 2) #(3 4))))
(defparameter *ps2* (make-point-sampling-fromtable #(#(1 2) #(3 4))))
(defparameter *ps3* (make-point-sampling-fromtable #(#(1 2) #(3 4) #(3 4))))
(defparameter *ps4* (make-point-sampling-fromtable #(#(1 2) #(3 5))))

(ps-eq *ps1* *ps2*) ;=> T
(ps-eq *ps1* *ps3*) ;=> nil
(ps-eq *ps1* *ps4*) ;=> nil
```

## Dependencies
* [point](https://github.com/thomashoullier/point)
* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* [cl-custom-hash-table](https://github.com/metawilm/cl-custom-hash-table)

## Further work
The following features would be useful:
* Find duplicates, and remove duplicates (note CL implements
  `#'remove-duplicates`).
* The equality predicate could be made more efficient with caching?

