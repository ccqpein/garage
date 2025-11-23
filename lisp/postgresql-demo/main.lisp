(ql:quickload '("postmodern" "sxql"))
(use-package :postmodern)

;;; need to make the table and 
(defun add-test-data ()
  (with-connection '("postgres" "postgres" "password" "localhost")
    (handler-case
        (create-database 'testdb
                         :limit-public-access t
                         :comment "This database is for testing silly theories")
      (CL-POSTGRES-ERROR:DUPLICATE-DATABASE (c)
        (declare (ignore c))
        (return-from add-test-data "test data has been added")))
    
    (query "-- Create a new table called 'products'
CREATE IF NOT EXIST TABLE products (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    price DECIMAL(10, 2) NOT NULL,
    description TEXT,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);")
    (query "-- Insert some sample data into the 'products' table
INSERT INTO products (name, price, description) VALUES
('Laptop Pro', 1200.00, 'High-performance laptop for professionals.'),
('Wireless Mouse', 25.50, 'Ergonomic wireless mouse with long battery life.'),
('USB-C Hub', 49.99, 'Multi-port adapter for modern laptops.'),
('External SSD 1TB', 99.00, 'Fast and portable storage solution.');")
    
    (query "-- Verify the data
SELECT * FROM products;"))
  )


;;; below just for testing the sxql

(defun main ()
  (with-connection '("postgres" "postgres" "password" "localhost")
    (let ((statement (sxql/statement::yield
                      (sxql/composer:->
                       (sxql:select (:id :name :price :description :created_at))
                       (sxql:from :products)))))
      (query statement))))
