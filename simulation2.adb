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

procedure Simulation2 is
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
	storageCopy: storageType := (0, 0, 0, 0, 0);

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
			productionTime: in Integer
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
				" produced."
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
			end loop;
		end loop;
	end producerType;

	task body customerType is
		consumptionGenerator: randomConsumption.Generator;
		setGenerator: randomSet.Generator;

		customerID: customerRange;
		setID: Integer;
		consumption: Integer;
		chosenSet: Integer;
	begin
		accept Start(customer: in customerRange; consumptionTime: in Integer) do
			randomConsumption.Reset(consumptionGenerator);
			randomSet.Reset(setGenerator);
			customerID := customer;
			consumption := consumptionTime;
		end Start;

		loop
			Put_Line("Customer: " & customerNames(customerID) & " is here.");
			delay Duration(randomConsumption.Random(consumptionGenerator));

			chosenSet := randomSet.Random(setGenerator);

			select
				delay Duration(customerWaitTime);
				Put_Line("Customer: " & customerNames(customerID) & " leaves. Reason: Wait time.");
			then abort
				Put_Line("Customer: " & customerNames(customerID) & " wants to buy " & setNames(chosenSet));
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

		function canDeliver(set: setRange; checkedStorage : storageType) return Boolean is
		begin
			for prod in productRange loop
				if checkedStorage(prod) < setComposition(set, prod) then
					return False;
				end if;
			end loop;

			return True;
		end canDeliver;
		
		function canTake(product: productRange) return Boolean is
			Free: Integer;
			tmpStorage: storageType;
			lack: array(setRange, productRange) of Integer;
			maxLack: array(productRange) of Integer;
			lacks: Integer;
		begin
			if inStorage >= storageCapacity then
				Put_Line(
					"BUFFER: Rejected product " & 
					productNames(Product) & 
					" because there is no space. "
				);
				return False;
			else
				Free := storageCapacity - inStorage;
				tmpStorage := storage;
				tmpStorage(product) := tmpStorage(product) + 1;

				for prod in productRange
				loop
					maxLack(prod) := 0;
				end loop;

				for aSet in setRange
				loop
					for prod in productRange
					loop
						lack(aSet, prod) := Integer'Max(0, setComposition(aSet, prod) - tmpStorage(prod));

						if lack(aSet, prod) > maxLack(prod) then
							maxLack(prod) := lack(aSet, prod);
						end if;
					end loop;
				end loop;
			end if;

			lacks := 0;

			for prod in productRange
			loop
				lacks := lacks + maxLack(prod);
			end loop;

			if Free >= lacks then
				return True;
			end if;
			
			-- to prevent product being too many times in storage:
			-- if product takes more or equal than storageCapacity/numOfProducts
			-- then we do not add it
			if storage(product) > storageCapacity / numOfProducts  then
				Put_Line(
					"BUFFER: Rejected product " & 
					productNames(product) & 
					" to prevent product being too many times in storage. "
				);
				return False;
			end if;

			-- to prevent buffer dead lock:
			-- if buffor is half filled, then we need to check 
			-- if new product will make assembly possible
			-- if not, then we do not add it, if it does then we add it
			if Free <= storageCapacity / 2 then
				-- copy storage array to ensure safety
				for productId in productRange'Range loop
					storageCopy(productId) := storage(productId);
				end loop;

				storageCopy(product) := storageCopy(product) + 1;

				for set in 1 .. numOfSets - 1 loop
					if canDeliver(setRange(set), storageCopy) then
						return True;
					end if;
				end loop;

				Put_Line(
					"BUFFER: Rejected product " & 
					productNames(product) & 
					" to prevent buffer deadlock. "
				);
				return False;
			end if;
			Put_Line(
				"BUFFER: Rejected product " &
				productNames(product)
			);
			return False;
		end canTake;

		procedure storageContent is
		begin
			Put_Line("");
			Put_Line("Buffer: storage content: ");
			for prod in productRange loop
				Put_Line(productNames(prod) & ": " & Integer'Image(storage(prod)));
			end loop;
			Put_Line("");
		end storageContent;

		procedure deleteProduct(product: in productRange) is
		begin
			storage(product) := storage(product) - 1;
			inStorage := inStorage - 1;
		end deleteProduct;

	begin
		Put_Line("Buffer: Buffor has started." & ASCII.LF);
		loop
			Put_Line("Buffer: Waiting for order...");
			select
				accept Deliver(set: in setRange; number: out Integer) do
					if canDeliver(set, storage) then
						Put_Line("Buffer: Delivered: " & setNames(set) & " nr " & Integer'Image(setID(set)));

						for prod in productRange loop
							storage(prod) := storage(prod) - setComposition(set, prod);
							inStorage := inStorage - setComposition(set, prod);
						end loop;

						number := setID(set);
						setID(set) := setID(set) + 1;
					else
						number := 0;
					end if;
				end Deliver;

				accept Take(product: in productRange; number: in Integer; isTaken: out Boolean) do
					--Put_Line("B [debug]: Przyjmiesz? " & Nazwa_productu(product) & " nr" & Integer'Image(Numer) & " na polke");
					if canTake(product) then
						storage(product) := storage(product) + 1;
						inStorage := inStorage + 1;
						isTaken := True;
						Put_Line("Buffer: Taken " & productNames(product) & ".");
						notTakenCounter := 0;
					else
						isTaken := False;

						notTakenCounter := notTakenCounter + 1;
						if notTakenCounter >= 3 then
							Put_Line("Buffer: product can not be taken if there is more than 3 of it in the storage at the moment. Removing the most common product.");

							maxProduct := 0;
							maxProductType := 1;
							for prod in productRange
							loop
								if storage(prod) > storage(maxProductType) then
									maxProduct := storage(prod);
									maxProductType := prod;
								end if;
							end loop;
							deleteProduct(maxProductType);
							notTakenCounter := 0;
							printStorageContent;
						end if;
					end if;
				end Take;
				or  delay Duration(3.0);				
					Put_Line("Buffer: No orders. Accepting products procedure...");
			end select;
			printStorageContent;
		end loop;
	end bufferType;

begin
	for product in 1 .. numOfProducts
	loop
		producers(product).Start(product, 10);
	end loop;

	for customer in 1 .. numOfCustomers
	loop
		customers(customer).Start(customer, 12);
	end loop;
end Simulation2;
