ingredient_characteristic('COTTAGE_CHEESE',calories,'GRAM',100,121).
ingredient_characteristic('COTTAGE_CHEESE',carbohydrates,'GRAM',100,1.8).
ingredient_characteristic('COTTAGE_CHEESE',fats,'GRAM',100,5.0).
ingredient_characteristic('COTTAGE_CHEESE',proteins,'GRAM',100,17.2).
ingredient_characteristic('EGG',calories,'NATURAL',1,157).
ingredient_characteristic('EGG',carbohydrates,'NATURAL',1,0.7).
ingredient_characteristic('EGG',fats,'NATURAL',1,10.9).
ingredient_characteristic('EGG',proteins,'NATURAL',1,12.7).
ingredient_characteristic('FLOUR',calories,'TABLESPOON',1,72).
ingredient_characteristic('FLOUR',carbohydrates,'TABLESPOON',1,16.0).
ingredient_characteristic('FLOUR',fats,'TABLESPOON',1,0.1).
ingredient_characteristic('FLOUR',proteins,'TABLESPOON',1,1.5).
ingredient_characteristic('GLAIR',calories,'NATURAL',1,44).
ingredient_characteristic('GLAIR',carbohydrates,'NATURAL',1,0.0).
ingredient_characteristic('GLAIR',fats,'NATURAL',1,0.0).
ingredient_characteristic('GLAIR',proteins,'NATURAL',1,11.1).
ingredient_characteristic('MILK',calories,'MILLILITER',100,59).
ingredient_characteristic('MILK',carbohydrates,'MILLILITER',100,4.7).
ingredient_characteristic('MILK',fats,'MILLILITER',100,3.2).
ingredient_characteristic('MILK',proteins,'MILLILITER',100,2.9).
ingredient_characteristic('OAT',calories,'GRAM',100,342).
ingredient_characteristic('OAT',carbohydrates,'GRAM',100,59.5).
ingredient_characteristic('OAT',fats,'GRAM',100,6.1).
ingredient_characteristic('OAT',proteins,'GRAM',100,12.3).
ingredient_characteristic('TEST',calories,'GRAM',100,100).
ingredient_characteristic('TEST',carbohydrates,'GRAM',100,100).
ingredient_characteristic('TEST',fats,'GRAM',100,100).
ingredient_characteristic('TEST',proteins,'GRAM',100,100).

ingredient_characteristic_query(Ingredient, Characteristic, Unit, Quantity, Value) :-
   ingredient_characteristic(Ingredient, Characteristic, Unit, BaseQuantity, BaseValue),
   Value is Quantity / BaseQuantity * BaseValue.
