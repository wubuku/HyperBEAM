## Best Practices for Modifying Large or Complex Files

To ensure safe and efficient file modifications, especially in large or duplicated-content files, always adhere to the **"Large Anchor, Small Change"** method.
1.  **Analyze First:** Read the file to fully understand its structure and identify the precise lines that need to change.
2.  **Identify a Large, Unique Anchor:** Instead of using just a few lines of context, select a large, structurally significant, and guaranteed-unique block of text that contains the lines you want to change.
    *   **Good Anchors:** An entire function definition, a whole class definition, a complete `CREATE TABLE` statement, or a unique section header with all of its content.
    *   **Bad Anchors:** A few lines of code that could appear elsewhere, generic import statements, or any small text block inside a duplicated section of the document.
3.  **Construct a Precise `replace` Call:**
    *   **`old_string`**: The entire unique anchor block you identified.
    *   **`new_string`**: A complete copy of the `old_string`, but with the small, targeted change made within it.
4.  **Verify After Every Change**: Immediately after every `replace` operation, use an external tool (`git diff` if available, or `read_file`) to verify that only the intended change was made. This step is critical for maintaining an accurate understanding of the file's state.