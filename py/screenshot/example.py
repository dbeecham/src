import pyautogui
import time

def main():
    screenshot = pyautogui.screenshot()
    print(type(screenshot))


if __name__ == "__main__":
    main()
