type Block 
  = Header (List Span) Int
  | Para (List Span)
  | Blockquote List Block
  | CodeBlock String
  | UnorderedList List ListItem
  | Raw String
  | Hr


type ListItem
  = Simple List Span
  | Paragraph List Block


type Span
  = Break
  | Text String
  | Code String
  | Link String String (Maybe String)
  | Image String String (Maybe String)
  | Emphasis List Span
  | String List Span


