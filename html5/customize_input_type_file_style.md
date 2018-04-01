# input type="file" のスタイルを変更する

`&lt;input type="file"&gt;` にはCSSが効かず、スタイルを変更できないので、
`input`要素を非表示にして、代わりに `label`要素で修飾し、それを CSS でスタイルづけする。


```elm
Html.label
    [ class "file_input_label" ]
    [ Html.text "Select a file from PC"
    , Html.input (FileReader.fileInput (FileReader.Text "utf-8") ReadFile) []
    ]
]
```

```css
.file_input_label {
    display: block;

    border: 1px solid darkgray;
    border-radius: 2px;
    background-color: royalblue;
    margin: 0.5em;
    padding: 5px;
    color: snow;
}

.file_input_label input {
    display: none;
}
```

## 参考

[[HTML5] &lt;input type="file"&gt;のデザインをもっと簡単に変更 | Qiita](https://qiita.com/yasumodev/items/c9f8e8f588ded6b179c9)
