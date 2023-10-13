-- A skeleton of a program for an assignment in programming languages
-- The students should rename the tasks of producers, consumers, and the buffer
-- Then, they should change them so that they would fit their assignments
-- They should also complete the code with constructions that lack there
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; 
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;


procedure Simulation is
   	Number_Of_Products: constant Integer := 5;
   	Number_Of_Assemblies: constant Integer := 3;
   	Number_Of_Consumers: constant Integer := 2;

   	subtype Product_Type is Integer range 1 .. Number_Of_Products;
   	subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   	subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;

   	Product_Name: constant array (Product_Type) of String(1 .. 6) := (
		"Bun   ", 
		"Cheese", 
		"Bacon ", 
		"Letuce", 
		"Tomato"
	);

   	Assembly_Name: constant array (Assembly_Type) of String(1 .. 15) := (
		"BLT Sandwich   ", 
		"Cheese Sandwich", 
		"Wege Sandwich  "
	);

   	package Random_Assembly is new Ada.Numerics.Discrete_Random(Assembly_Type);

   	type My_Str is new String(1 .. 256);

   	-- Producer produces determined product
   	task type Producer is
      	-- Give the Producer an identity, i.e. the product type
    	entry Start(Product: in Product_Type; Production_Time: in Integer);
   	end Producer;

   	-- Consumer gets an arbitrary assembly of several products from the buffer
   	task type Consumer is
      	-- Give the Consumer an identity
      	entry Start(Consumer_Number: in Consumer_Type; Consumption_Time: in Integer);
   	end Consumer;

   	-- In the Buffer, products are assemblied into an assembly
   	task type Buffer is
      	-- Accept a product to the storage provided there is a room for it
      	entry Take(Product: in Product_Type; Number: in Integer);
      	-- Deliver an assembly provided there are enough products for it
      	entry Deliver(Assembly: in Assembly_Type; Number: out Integer; Did_Assembly: out Boolean);
   	end Buffer;

   	Producer_Array: array ( 1 .. Number_Of_Products ) of Producer;
   	Consumer_Array: array ( 1 .. Number_Of_Consumers ) of Consumer;
   	The_Buffer: Buffer; -- The_Buffer is a var of subtype mark Buffer

   	task body Producer is
		subtype Production_Time_Range is Integer range 3 .. 6;

		package Random_Production is new Ada.Numerics.Discrete_Random(Production_Time_Range);

		Production_Time_Generator: Random_Production.Generator;    -- generator liczb losowych
		Product_Type_Number: Integer;
		Product_Number: Integer;
		Production: Integer;

	begin
		accept Start(Product: in Product_Type; Production_Time: in Integer) do
			Random_Production.Reset(Production_Time_Generator);    -- start random number generator
			Product_Number := 1;
			Product_Type_Number := Product;
			Production := Production_Time;
		end Start;

		Put_Line(
			"PRODUCER: Started producer of " & 
			Product_Name(Product_Type_Number)
		);

		loop
			delay Duration(Random_Production.Random(Production_Time_Generator)); -- symuluj produkcję

			Put_Line(
				"PRODUCER: Produced product " & 
				Product_Name(Product_Type_Number) & 
				" number "  & 
				Integer'Image(Product_Number)
			);

			-- Accept for storage
			The_Buffer.Take(Product_Type_Number, Product_Number);
			Product_Number := Product_Number + 1;
		end loop;
	end Producer;


   	task body Consumer is
		subtype Consumption_Time_Range is Integer range 4 .. 8;
		package Random_Consumption is new Ada.Numerics.Discrete_Random(Consumption_Time_Range);
	
		Consumption_Time_Generator: Random_Consumption.Generator;	--  random number generator (time)
		Assembly_Type_Generator: Random_Assembly.Generator;	--  also (assemblies)
		Consumer_ID: Consumer_Type;
		Assembly_Number: Integer;
		Consumption: Integer;
		Assembly_Type: Integer;
		Did_Assembly: Boolean;
		Consumer_Name: constant array (1 .. Number_Of_Consumers) of String(1 .. 6) := (
			"Michal", 
			"Wojtek"
		);

	begin 
		accept Start(Consumer_Number: in Consumer_Type; Consumption_Time: in Integer) do
			Random_Consumption.Reset(Consumption_Time_Generator);	--  ustaw generator
			Random_Assembly.Reset(Assembly_Type_Generator);		--  też
			Consumer_ID := Consumer_Number;
			Consumption := Consumption_Time;
			Did_Assembly := false;
		end Start;

		Put_Line("CONSUMER: Started consumer " & Consumer_Name(Consumer_ID));

		loop
			delay Duration(Random_Consumption.Random(Consumption_Time_Generator)); --  simulate consumption

			Assembly_Type := Random_Assembly.Random(Assembly_Type_Generator);

			-- take an assembly for consumption
			Put_Line("CONSUMER: " & Consumer_Name(Consumer_ID) & ": wants " & Assembly_Name(Assembly_Type));

			The_Buffer.Deliver(Assembly_Type, Assembly_Number, Did_Assembly);
			if Did_Assembly then
				Put_Line(
					"CONSUMER: " & Consumer_Name(Consumer_ID) & ": taken assembly " &
					Assembly_Name(Assembly_Type) & " number " &
					Integer'Image(Assembly_Number)
				);
			-- else
			--	Put_Line("BUFFER: Lacking products for assembly ");					
			end if;
		end loop;
	end Consumer;

   	task body Buffer is
      	Storage_Capacity: constant Integer := 8;
      	type Storage_type is array (Product_Type) of Integer;
      	Storage: Storage_type := (0, 0, 0, 0, 0);
      	Storage_Copy: Storage_type := (0, 0, 0, 0, 0);
      	Assembly_Content: array(Assembly_Type, Product_Type) of Integer := 
		(
			(2, 1, 2, 1, 2),
	    	(2, 2, 0, 1, 0),
	    	(1, 1, 2, 0, 1)
		);
      	Max_Assembly_Content: array(Product_Type) of Integer;
      	Assembly_Number: array(Assembly_Type) of Integer := (1, 1, 1);
      	In_Storage: Integer := 0;

		procedure Setup_Variables is 
		begin
			for Product_ID in Product_Type loop
				Max_Assembly_Content(Product_ID) := 0;
				for Assembly_ID in Assembly_Type loop
					if Assembly_Content(Assembly_ID, Product_ID) > Max_Assembly_Content(Product_ID) then
						Max_Assembly_Content(Product_ID) := Assembly_Content(Assembly_ID, Product_ID);
					end if;
				end loop;
			end loop;
		end Setup_Variables;

		function Can_Deliver(Assembly: Assembly_Type; Checked_Storage : Storage_type) return Boolean is
		begin
			for Product_ID in Product_Type loop
				if Checked_Storage(Product_ID) < Assembly_Content(Assembly, Product_ID) then
					return False;
				end if;
			end loop;
			return True;
		end Can_Deliver;

    	function Can_Accept(Product: Product_Type) return Boolean is
			Free: Integer;		--  free room in the storage
			-- how many products are for production of arbitrary assembly
			Lacking: array(Product_Type) of Integer;
			-- how much room is needed in storage to produce arbitrary assembly
			Lacking_room: Integer;
			MP: Boolean;			--  can accept
		begin
			if In_Storage >= Storage_Capacity then
				return False;
			end if;

			-- There is free room in the storage
			Free := Storage_Capacity - In_Storage;
			MP := True; -- Co to znaczy MP?

			for Product_ID in Product_Type loop
				if Storage(Product_ID) < Max_Assembly_Content(Product_ID) then
					MP := False;
				end if;
			end loop;

			if MP then
				return True;	--  storage has products for arbitrary
				--  assembly
			end if;
			if Integer'Max(0, Max_Assembly_Content(Product) - Storage(Product)) > 0 then
				-- exactly this product lacks
				return True;
			end if;

			Lacking_room := 1;		--  insert current product
			for Product_ID in Product_Type loop
				Lacking(Product_ID) := Integer'Max(0, Max_Assembly_Content(Product_ID) - Storage(Product_ID));
				Lacking_room := Lacking_room + Lacking(Product_ID);
			end loop;

			if Free >= Lacking_room then
				-- there is enough room in storage for arbitrary assembly
				return True;
			end if;
			-- if buffor is half filled, then we need to check 
			-- if new product will make assembly possible
			-- if not, then we do not add it, if it does then we add it
			-- Check if the buffer is half filled
			if Free <= Storage_Capacity / 2 then
				-- copy storage array to ensure safety
				for Product_ID in Product_Type'Range loop
					Storage_Copy(Product_ID) := Storage(Product_ID);
				end loop;

				Storage_Copy(Product) := Storage_Copy(Product) + 1;

				for Assembly_ID in 1 .. Number_Of_Assemblies - 1 loop
					if Can_Deliver(Assembly_Type(Assembly_ID), Storage_Copy) then
						return True;
					end if;
				end loop;

				Put_Line(
					"BUFFER: Rejected product " & 
					Product_Name(Product) & 
					" to prevent buffer overflow. "
				);
			end if;
			
			return False;
      	end Can_Accept;

      	procedure Print_Storage_Contents is
		begin
			for Product_ID in Product_Type loop
				Put_Line(
					"Storage contents: " &
					 Integer'Image(Storage(Product_ID)) &
					" " & Product_Name(Product_ID)
				);
			end loop;
      	end Print_Storage_Contents;

	begin
		Put_Line("BUFFER: Buffer started");
		Setup_Variables;
		loop
			select
				accept Take(Product: in Product_Type; Number: in Integer) do
					if Can_Accept(Product) then
						Put_Line(
							"BUFFER: Accepted product " &
							Product_Name(Product) &
							" number " &
							Integer'Image(Number)
						);

						Storage(Product) := Storage(Product) + 1;
						In_Storage := In_Storage + 1;
					else
						Put_Line(
							"BUFFER: Rejected product " &
							Product_Name(Product) & 
							" number " &
							Integer'Image(Number)
						);
					end if;
				end Take;
			or
				accept Deliver(Assembly: in Assembly_Type; Number: out Integer; Did_Assembly: out Boolean) do
					if Can_Deliver(Assembly, Storage) then
						Put_Line(
							"BUFFER: Delivered assembly " & 
							Assembly_Name(Assembly) & 
							" number " &
							Integer'Image(Assembly_Number(Assembly))
						);

						for Product_ID in Product_Type loop
							Storage(Product_ID) := Storage(Product_ID) - Assembly_Content(Assembly, Product_ID);
							In_Storage := In_Storage - Assembly_Content(Assembly, Product_ID);
						end loop;

						Number := Assembly_Number(Assembly);
						Assembly_Number(Assembly) := Assembly_Number(Assembly) + 1;
						Did_Assembly := true;
					else
						Put_Line("BUFFER: Lacking products for assembly " & Assembly_Name(Assembly));
						Number := 0;
						Did_Assembly := false;
					end if;
				end Deliver;
			end select;
			Print_Storage_Contents;
		end loop;
	end Buffer;
	
begin
	for Product_ID in 1 .. Number_Of_Products loop
		Producer_Array(Product_ID).Start(Product_ID, 10);
	end loop;
	for Consumer_ID in 1 .. Number_Of_Consumers loop
		Consumer_Array(Consumer_ID).Start(Consumer_ID,12);
	end loop;
end Simulation;


