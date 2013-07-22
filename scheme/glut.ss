(require-extension gl glut)

(glut:CreateWindow "simple")

(glut:DisplayFunc
  (lambda ()
    (gl:Clear gl:COLOR_BUFFER_BIT)
    (gl:Begin gl:POLYGON)
    (gl:Vertex2f -0.5 -0.5)
    (gl:Vertex2f -0.5 0.5)
    (gl:Vertex2f 0.5 0.5)
    (gl:Vertex2f 0.5 -0.5)
    (gl:End)
    (gl:Flush)))

(gl:ClearColor 0 0 0 1)
(glut:MainLoop)
