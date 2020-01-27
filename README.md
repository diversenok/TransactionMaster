## Transaction Master

**Transaction Master** is an experimental tool for Windows that allows you to switch other processes into the context of an NTFS transaction. A **transaction** is a Windows kernel object that encapsulates a set of operations (in this case — file-system operations) into a single entity. This entity provides isolation, which means that changes made within a transaction are not visible to the rest of the system, and atomicity, which means the owner can **commit** (make permanent) or **rollback** it as a whole. The last part also allows the system to maintain consistency. For more information see [ACID](https://en.wikipedia.org/wiki/ACID) and [TxF](https://en.wikipedia.org/wiki/TxF).

![Overview screenshot](https://habrastorage.org/webt/p0/-s/fi/p0-sfitxnzzpywfqxuc4arx1fps.png)

## How it works

To understand the program's capabilities and limitations, you can consult with my article:

- In English: [How to Make Any Process Work With Transactional NTFS: My First Step to Writing a Sandbox for Windows](https://habr.com/en/post/485788/).
- In Russian: [Заставляем любой процесс работать с транзакционной NTFS: мой первый шаг к созданию песочницы для Windows](https://habr.com/ru/post/485784/).

## Downloads

See [releases](https://github.com/diversenok/TransactionMaster/releases) page

**Note:** if you have a 64-bit version of Windows, use the x64 version of the program. It supports both native and WoW64 target processes.

The program was tested on Windows 7 and 10. It might also work on Vista.

Key        | Value
---------- | -----
Author     | © diversenok
Email      | diversenok@gmail.com (English and Russian are suitable)
Compiled   | Embarcadero Delphi 10.3
Version    | 1.1.25.0
Date       | Jan 25, 2020

------------------------------------------------------------------------------

    Copyright (C) 2019-2020 diversenok

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.