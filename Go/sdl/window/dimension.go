package window

type Dimension struct {
	width, height int
}

func NewDimension(w, h int) *Dimension {
	return &Dimension{
		width:  w,
		height: h,
	}
}
