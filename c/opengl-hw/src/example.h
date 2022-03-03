#pragma once

#include <GL/glew.h>
#include <GLFW/glfw3.h>

struct example_s {
    GLuint program_id;
    GLuint vertex_shader_id;
    GLuint fragment_shader_id;
    GLFWwindow * window;
};
