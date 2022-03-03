#include <stdio.h>
#include <stdlib.h>
#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include "example.h"

int example_init_shaders (
)
{
    int ret = 0;
    

    return 0;
}


int example_init_glfw (
    struct example_s * example
)
{

    int ret = 0;

    ret = glfwInit();
    if (1 != ret) {
        printf("%s:%d:%s: glfwInit returned %d\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }

    return 0;
}


int example_init_glew (
    struct example_s * example
)
{

    int ret = 0;

    ret = glewInit();
    if (GLEW_OK != ret) {
        printf("%s:%d:%s: glewInit returned %d\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }
    
    return 0;
}


int example_init_window (
    struct example_s * example
)
{
    glfwWindowHint(GLFW_SAMPLES, 4); // 4x antialiasing
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3); // we want version 3.3
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE); // we dont want the old opengl

    example->window = glfwCreateWindow(1024, 768, "example", NULL, NULL);
    if (NULL == example->window) {
        printf("%s:%d:%s: glfwCreateWindow returned NULL\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    glfwMakeContextCurrent(example->window);
    glfwSetInputMode(example->window, GLFW_STICKY_KEYS, GL_TRUE);

    return 0;
}


int example_init_gl_vertex_shaders (
    struct example_s * example
)
{
    GLint result = 0;
    int info_log_length = 0;
    char const * const vertex_shader = {
        (const char*){
            "#version 330 core\n"
            "layout(location = 0) in vec3 vertexPosition_modelspace;\n"
            "void main(){\n"
                "gl_Position.xyz = vertexPosition_modelspace;\n"
                "gl_Position.w = 1.0;\n"
            "}"
            "\0"
        }
    };

    example->vertex_shader_id = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(example->vertex_shader_id, 1, &vertex_shader, NULL);
    glCompileShader(example->vertex_shader_id);

    // did it work?
    glGetShaderiv(example->vertex_shader_id, GL_COMPILE_STATUS, &result);
    glGetShaderiv(example->vertex_shader_id, GL_INFO_LOG_LENGTH, &info_log_length);
    if (0 < info_log_length) {
        char buf[2048];
        glGetShaderInfoLog(example->vertex_shader_id, 2048, NULL, buf);
        printf("%s:%d:%s: glCompileShader: %.*s\n", __FILE__, __LINE__, __func__, 2048, buf);
        return -1;
    }
     

    return 0;
}


int example_init_gl_fragment_shaders (
    struct example_s * example
)
{
    GLint result = 0;
    int info_log_length = 0;
    char const * const fragment_shader = {
        (const char*){
            "#version 330 core\n"
            "out vec3 color;\n"
            "void main() {\n"
                "color = vec3(1,0,0);\n"
            "}\n"
            "\0"
        }
    };

    // compile vertex shader
    example->fragment_shader_id = glCreateShader(GL_FRAGMENT_SHADER);


    glShaderSource(example->fragment_shader_id, 1, &fragment_shader, NULL);
    glCompileShader(example->fragment_shader_id);

    // did it work?
    glGetShaderiv(example->fragment_shader_id, GL_COMPILE_STATUS, &result);
    glGetShaderiv(example->fragment_shader_id, GL_INFO_LOG_LENGTH, &info_log_length);
    if (0 < info_log_length) {
        char buf[2048];
        glGetShaderInfoLog(example->fragment_shader_id, 2048, NULL, buf);
        printf("%s:%d:%s: glCompileShader: %.*s\n", __FILE__, __LINE__, __func__, 2048, buf);
        return -1;
    }

    return 0;
}


int example_init_gl (
    struct example_s * example
)
{
    int ret = 0;
    GLint result = 0;
    int info_log_length = 0;

    example->program_id = glCreateProgram();

    ret = example_init_gl_vertex_shaders(example);
    if (-1 == ret) {
        printf("%s:%d:%s: example_init_gl_vertex_shaders returned -1\n", __FILE__, __LINE__, __func__);
        return -1; 
    }

    ret = example_init_gl_fragment_shaders(example);
    if (-1 == ret) {
        printf("%s:%d:%s: example_init_gl_fragment_shaders returned -1\n", __FILE__, __LINE__, __func__);
        return -1; 
    }


    glAttachShader(example->program_id, example->vertex_shader_id);
    glAttachShader(example->program_id, example->fragment_shader_id);
    glLinkProgram(example->program_id);

    // did it work?
    glGetProgramiv(example->program_id, GL_LINK_STATUS, &result);
    glGetProgramiv(example->program_id, GL_INFO_LOG_LENGTH, &info_log_length);
    if (0 < info_log_length) {
        char buf[2048];
        glGetProgramInfoLog(example->program_id, 2048, NULL, buf);
        printf("%s:%d:%s: program log=%.*s\n", __FILE__, __LINE__, __func__, 2048, buf);
        return -1;
    }

    glDetachShader(example->program_id, example->vertex_shader_id);
    glDetachShader(example->program_id, example->fragment_shader_id);
    
    glDeleteShader(example->vertex_shader_id);
    glDeleteShader(example->fragment_shader_id);

    return 0;
}


int example_init (
    struct example_s * example
)
{

    int ret = 0;

    ret = example_init_glfw(example);
    if (-1 == ret) {
        printf("%s:%d:%s: example_init_glfw returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = example_init_window(example);
    if (-1 == ret) {
        printf("%s:%d:%s: example_init_window returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = example_init_glew(example);
    if (-1 == ret) {
        printf("%s:%d:%s: example_init_glew returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = example_init_gl(example);
    if (-1 == ret) {
        printf("%s:%d:%s: example_init_gl returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }


    return 0;
}


int main (
    int argc,
    char const* argv[]
)
{
    int ret = 0;
    GLint result = 0;
    struct example_s example = {0};
    int info_log_length = 0;


    ret = example_init(&example);
    if (-1 == ret) {
        printf("%s:%d:%s: example_init returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }


    GLuint vertex_array_id;
    glGenVertexArrays(1, &vertex_array_id);
    glBindVertexArray(vertex_array_id);

    // points of a triangle
    const GLfloat g_vertex_buffer_data[] = {
        -1.0f, -1.0f, 0.0f,
        1.0f, -1.0f, 0.0f,
        0.0f, 1.0f, 0.0f
    };

    GLuint vertexbuffer;
    glGenBuffers(1, &vertexbuffer);
    glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(g_vertex_buffer_data), g_vertex_buffer_data, GL_STATIC_DRAW);


    do {
        glClear(GL_COLOR_BUFFER_BIT);

        glUseProgram(example.program_id);

        // draw stuff
        glEnableVertexAttribArray(0);
        glBindBuffer(GL_ARRAY_BUFFER, vertexbuffer);
        glVertexAttribPointer(
            /* attribute id = */ 0,
            /* size = */ 3,
            /* type = */ GL_FLOAT,
            /* normalized = */ GL_FALSE,
            /* stride = */ 0,
            /* offset = */ NULL
        );

        // draw triangle
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glDisableVertexAttribArray(0);


        glfwSwapBuffers(example.window);
        glfwPollEvents();
    }
    while (glfwGetKey(example.window, GLFW_KEY_ESCAPE) != GLFW_PRESS && glfwWindowShouldClose(example.window) == 0);

    return 0;
}
