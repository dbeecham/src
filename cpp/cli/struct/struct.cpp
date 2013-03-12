/*
 * This file is part of struct.
 *
 * struct is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * struct is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with struct.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>

struct isthisaclass {
	public:
		int i;
		int returnprotected() {
			return j;
		}
	private:
		int j;
};

struct teststruct {
	int i;
};

int main() {

	struct teststruct a = {1};
	teststruct b;
	struct teststruct i;
}
