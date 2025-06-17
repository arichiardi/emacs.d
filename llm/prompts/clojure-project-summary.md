I'd like you to create an LLM-friendly project summary for this codebase. 

The root directory of this codebase/project is at:
{{root-directory}}

First try to `read_file` the `{{root-directory}}/PROJECT_SUMMARY.md`

IF there is no `PROJECT_SUMMARY.md` THEN

Please analyze the key files, dependencies, and structure, then generate a `PROJECT_SUMMARY.md` in `{{root-directory}}` that includes:

A brief overview of what the project does
Key file paths with descriptions of their purpose
Important dependencies with versions and their roles
Available tools/functions/APIs with examples of how to use them
The overall architecture and how components interact
Implementation patterns and conventions used throughout the code
Development workflow recommendations
Extension points for future development

Structure this summary to help an LLM coding assistant quickly understand the project and provide effective assistance with minimal additional context.

ELSE IF a `PROJECT_SUMMARY.md` already exists THEN 

Please use the `read_file` tool to read it and then update it with any new information that we have learned in this current chat.

