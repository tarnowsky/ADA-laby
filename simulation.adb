--  Product_Name: constant array (Product_Type) of String(1 .. 6) := (
--  	"Bun   ", 
--  	"Cheese", 
--  	"Bacon ", 
--  	"Letuce", 
--  	"Tomato"
--  );

--  Assembly_Name: constant array (Assembly_Type) of String(1 .. 15) := (
--  	"BLT Sandwich   ", 
--  	"Cheese Sandwich", 
--  	"Wege Sandwich  "
--  );

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics;
use Ada.Numerics;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure Simulation is
	numOfProducts: constant Integer := 5;
	numOfSets: constant Integer := 3;
	numOfCustomers: constant Integer := 2;
	customerWaitTime: constant Integer := 10;
	bufforCapacity: constant Integer := 10;

	subtype productionTimeRange is Integer range 5 .. 10;
	subtype consumptionTimeRange is Integer range 5 .. 10;
	subtype productRange is Integer range 1 .. numOfProducts;
	subtype setRange is Integer range 1 .. numOfSets;
	subtype customerRange is Integer range 1 .. numOfCustomers;

	type storageType is array (productRange) of Integer;

	productNames: constant array (productRange) of String (1 .. 6) := (
		"Bun   ", 
		"Cheese", 
		"Bacon ", 
		"Letuce", 
		"Tomato"
	);

	setNames: constant array (setRange) of String (1 .. 15) := (
		"BLT Sandwich   ", 
		"Cheese Sandwich", 
		"Wege Sandwich  "
	);

	customerNames: constant array(customerRange) of String(1 .. 6) := (
		"Michal",
		"Wojtek"
	);

	storage: storageType := (0, 0, 0, 0, 0);

	setComposition: array(setRange, productRange) of Integer := (
		(2, 1, 2, 1, 2),
		(2, 2, 0, 1, 0),
		(1, 1, 2, 0, 1)
	);

	setID: array(setRange) of Integer := (1, 1, 1);

	package randomConsumption is new Discrete_Random(consumptionTimeRange);
	package randomSet is new Discrete_Random(setRange);

	task type producerType is
		entry Start(
			product: in productRange;
			productionTime: in Integer;
		);
	end producerType;

	task type customerType is
		entry Start(customerID: in customerRange; consumptionTime: in Integer);
	end customerType;

	task type bufforType is
		entry Take(product: in productRange; number: in Integer; Czy_Przyjeto: out Boolean);
		entry Give(Zestaw: in Zakres_Ciast; Numer: out Integer);
	end bufforType;
	