# AutoComplete
Team Members: 
Sabrina Liu, 
Kathleen Chen,
Xueqing Tsang,
Cici Zhou, 
Ivy Zhou <br />
other helpful resources used are in the AUTHORS.md file
## Demo Video:
[AutoComplete Demo Video](https://www.youtube.com/watch?v=GLEakwbu01g)

## Description: 
- Uses red font to provide autocomplete suggestions for words and sentences that the user types.
- Suggests correct spellings for the word being currently typed.
- Uses AI through Hugging Face API fetching to suggest how to finish the current sentence everytime the user clicks space.
- Pressing tab autocompletes the current word in black with the top suggested spelling. Pressing tab autocompletes the rest of the sentence with the current rest of sentence suggestion in black.
- Repeatedly pressing space can generate more sentence completion suggestions in red.
- Text can be saved to a txt file by pressing the save button. Progress can be restored to the GUI when reopening the application by clicking the retrieve button.
- The press me button plays a fun cat animation.

## How to Launch:
1. Install VSCode
2. Have dune installed either on Mac Terminal or Ubuntu Unix.
3. Follow the INSTALL.md file and downlaod all necessary libraries in whichever terminal that has dune installed.
4. Navigate to the folder containing this git repo code in the terminal being used.
5. Create a account on huggingface.co and create a free access token.
6. Open the code in VSCode and in the inference.ml file set the variable api_key equal to your own access token as a string.
7. Execute the main file in the terminal with this command:dune exec bin/main.exe.
8. The GUI should have been launched and start typing to use the application.

## Images:
![image](https://github.com/user-attachments/assets/4ba44404-842c-427c-b796-fcc5241ae501)
![image](https://github.com/user-attachments/assets/dbcf2b36-42fe-429e-8b1a-6004aeaba9a0)
![image](https://github.com/user-attachments/assets/8cff74c2-d598-44b5-b4b4-bfaaeb130ab0)


