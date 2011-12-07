(:viewport
  :background-image "./test/s-b-ii.png")
(:wrapper
  :flow vertical)
(:fill-box
  :min-size #(500 250) :margin-left 50)
(.ball
  :shape round
  :min-size #(100 100)
  :max-size #(100 100)
  :background-color #(1 0.8 0.6 0.5)) 
(:msg-box :shape round :background-color #(0.8 0.8 1 0.5) :min-size #(500 100) :margin-left 50 :border-color-top #(0.8 0.8 1 0.9) :border-style-top solid :border-style-bottom solid :border-color-bottom #(0.8 0.8 1 0.9) :max-size #(500 100))

