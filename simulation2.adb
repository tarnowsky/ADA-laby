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
	storageCapacity: constant Integer := 10;

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
		entry Start(customer: in customerRange; consumptionTime: in Integer);
	end customerType;

	task type bufferType is
		entry Take(product: in productRange; number: in Integer; isTaken: out Boolean);
		entry Deliver(set: in setRange; number: out Integer);
	end bufferType;
	
	producers: array(1 .. numOfProducts) of producerType;
	customers: array(1 .. numOfCustomers) of customerType;
	buffer: bufferType;

	task body producerType is
		package randomProduction is new Discrete_Random(productionTimeRange);

		productionGenerator: randomProduction.Generator;
		productID: Integer := 1;
		productTypeID: Integer;
		production: Integer;
		isTaken: Boolean := False;

	begin
		accept Start(product: in productRange; productionTime: in Integer) do
			randomProduction.Reset(productionGenerator);
			productTypeID := product;
			production := productionTime;
		end Start;

		Put_Line("Producer: Production of " & productNames(productTypeID) & " has started.");

		loop
			delay Duration(randomProduction.Random(productionGenerator));
			Put_Line(
				"Producer: A " & 
				productNames(productTypeID) & 
				" number " &
				Integer'Image(productID) &
				" was produced."
			);

			loop
				buffer.Take(productTypeID, productID, isTaken);
				if isTaken = False then
					Put_Line(
						"Producer: Storage full. Can not take " &
						productNames(productTypeID) &
						". Waiting procedure... (4s)"
					);
					delay Duration(4.0);
				else
					productID := productID + 1;
				end if;
				exit;
			end loop
		end loop;
	end producerType;

	task body customerType is
		consumptionGenerator: randomConsumption.Generator;
		setGenerator: randomSet.Generator;
		customerID: customerRange;
		setID: Integer;
		consumption: Integer;
		chosenSet: Integer
	begin
		accept Start(customer: in customerRange; consumptionTime: in Integer) do
			randomConsumption.Reset(consumptionGenerator);
			randomSet.Reset(setGenerator);
			customerID := customer;
			consumption := consumptionTime;
		end Start;

		loop
			Put_Line("Customer: " & customerNames(customerID) & " is here.");
			delay Duration(randomConsumption(consumptionGenerator));

			chosenSet := randomSet.Random(setGenerator);

			select
				delay Duration(customerWaitTime);
				Put_Line("Customer: " & customerNames(customerID) & " leaves. Reason: Wait time.");
			then abort
				Put_Line("Customer: " & customerNames(customerID) & " wants to buy " & setNames(chosenSet););
				loop
					buffer.Deliver(chosenSet, setID);
					if setID = 0 then
						Put_Line("Customer: Lack of products for " & setNames(chosenSet) & " for " & customerNames(customerID) & ". Waiting procedure... (4s)");
						delay Duration(4.0);
					else
						Put_Line("Customer: " & customerNames(customerID) & " bought " & setNames(chosenSet));
						Put_Line("Customer: " & customerNames(customerID) & " leaves.");
						exit;
					end if;
				end loop;
			end select;
			delay Duration(randomConsumption.Random(consumptionGenerator));
		end loop;
	end customerType;

	task body bufferType is
		inStorage: Integer := 0;
		notTakenCounter: Integer := 0;
		maxProduct: Integer := 0;
		maxProductType: productRange := 1;

		function canBeTaken(product: productRange) return Boolean is
			Free: Integer;
			tmpStorage: storageType;
			lack: array(setRange, productRange) of Integer;
			maxLack: array(productRange) of Integer;
			lacks: Integer;
		begin
			if inStorage >= storageCapacity then
				return False;
			else
				Free := storageCapacity - inStorage;
				tmpStorage := storage;
				tmpStorage(product) := tmpStorage(product) + 1;

				for prod in productRange
				loop
					maxLack(prod) := 0;
				end loop

				for set in Zakres_Ciast
				loop
					for W in Zakres_Wyrobow
					loop
						Brak(Z, W) := Integer'Max(0, Sklad_Zestawu(Z, W) - Tmp_Mag(W));

						if Brak(Z, W) > Max_Brak(W) then
							Max_Brak(w) := Brak(Z, W);
						end if;
					end loop;
				end loop;
			end if;


