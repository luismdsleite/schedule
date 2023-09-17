module SelectLists.Category exposing (..)

import Html.Styled
import ScheduleObjects.Restriction exposing (Category(..), allCategories, categoryToPortugueseString)
import Select exposing (basicMenuItem)


type alias CategoryList =
    { selectState : Select.State
    , items : List (Select.MenuItem Category)
    , selectedCategory : Category
    }


initCategoryList : Category -> CategoryList
initCategoryList category =
    { selectState =
        Select.initState (Select.selectIdentifier "Category")
    , items =
        List.map (\cat -> basicMenuItem { item = cat, label = categoryToPortugueseString cat }) allCategories
    , selectedCategory = category
    }


renderCategorySelect : CategoryList -> Html.Styled.Html (Select.Msg Category)
renderCategorySelect categoryList =
    Select.view
        ((Select.single <| Just (basicMenuItem { item = categoryList.selectedCategory, label = categoryToPortugueseString categoryList.selectedCategory }))
            |> Select.state categoryList.selectState
            |> Select.menuItems categoryList.items
            |> Select.placeholder "Categoria"
        )


setCategoryList : CategoryList -> { b | categoryList : CategoryList } -> { b | categoryList : CategoryList }
setCategoryList categoryList a =
    { a | categoryList = categoryList }


setCategoryListSelect : Category -> { b | selectedCategory : Category } -> { b | selectedCategory : Category }
setCategoryListSelect category categoryList =
    { categoryList | selectedCategory = category }
